# Renders the hero screenshot: runs remax in a pty over the seeded
# /tmp/remax-hero state, reconstructs the final frame with pyte, writes
# it as HTML, and screenshots it with headless Chrome at 2x.
#
# Driven by scripts/hero-shot; expects the hero state to already exist
# (scripts/hero --no-launch builds it).

import html
import os
import pty
import shutil
import signal
import struct
import subprocess
import sys
import time

import fcntl
import termios

import pyte

COLS, ROWS = 170, 45
FONT_PX, LINE_PX, PAD_PX = 13, 17, 14

DEF_FG = '#e8e8d3'
DEF_BG = '#151515'

# jellybeans-adjacent fallbacks for the few cells vty emits as named
# ANSI colors instead of truecolor
NAMED = {
    'black': '#151515', 'red': '#cf6a4c', 'green': '#99ad6a',
    'brown': '#fad07a', 'yellow': '#fad07a', 'blue': '#8197bf',
    'magenta': '#a390f0', 'cyan': '#8fbfdc', 'white': '#e8e8d3',
    'brightblack': '#535353', 'brightred': '#cf6a4c',
    'brightgreen': '#99ad6a', 'brightyellow': '#fad07a',
    'brightblue': '#8197bf', 'brightmagenta': '#a390f0',
    'brightcyan': '#8fbfdc', 'brightwhite': '#ffffff',
}


def capture(hero_dir, remax_bin):
    pid, fd = pty.fork()
    if pid == 0:
        os.chdir(os.path.join(hero_dir, 'repo'))
        os.environ.update(
            TERM='xterm-256color',
            COLORTERM='truecolor',
            XDG_STATE_HOME=os.path.join(hero_dir, 'state'),
            XDG_CONFIG_HOME=os.path.join(hero_dir, 'config'),
        )
        os.execvp(remax_bin, [remax_bin])

    os.set_blocking(fd, False)
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack('HHHH', ROWS, COLS, 0, 0))

    # capture the full paint from startup and never send :q — vty's exit
    # cleanup frames would corrupt the emulated grid
    buf = b''
    end = time.time() + 4.0
    while time.time() < end:
        try:
            data = os.read(fd, 65536)
            if data:
                buf += data
        except (BlockingIOError, OSError):
            time.sleep(0.02)
    os.kill(pid, signal.SIGKILL)
    try:
        os.waitpid(pid, 0)
    except ChildProcessError:
        pass
    return buf


def css_color(color, default):
    if color == 'default':
        return default
    if color in NAMED:
        return NAMED[color]
    return '#' + color


def to_html(buf):
    screen = pyte.Screen(COLS, ROWS)
    pyte.ByteStream(screen).feed(buf)

    rows_html = []
    for y in range(ROWS):
        spans = []
        run_style, run_text = None, []

        def flush():
            if run_text:
                fg, bg, bold = run_style
                weight = ';font-weight:600' if bold else ''
                spans.append(
                    '<span style="color:%s;background:%s%s">%s</span>'
                    % (fg, bg, weight, html.escape(''.join(run_text)))
                )

        for x in range(COLS):
            cell = screen.buffer[y][x]
            fg = css_color(cell.fg, DEF_FG)
            bg = css_color(cell.bg, DEF_BG)
            if cell.reverse:
                fg, bg = bg, fg
            style = (fg, bg, cell.bold)
            if style != run_style:
                flush()
                run_style, run_text = style, []
            run_text.append(cell.data or ' ')
        flush()
        rows_html.append(''.join(spans))

    # each row is a fixed-height box: glyphs that fall back to another
    # font (meter blocks, bullets) otherwise inflate their line and
    # shift the grid
    return (
        '<!doctype html><meta charset="utf-8"><style>'
        'html,body{margin:0;padding:0;background:%s}'
        'main{padding:%dpx;background:%s;display:inline-block}'
        'main div{height:%dpx;line-height:%dpx;overflow:hidden;'
        'white-space:pre;font:%dpx/%dpx "Source Code Pro",'
        '"DejaVu Sans Mono",monospace}'
        # spans as full-height inline blocks: an inline box's background
        # can undershoot the row box and leave hairline seams between rows
        'main span{display:inline-block;height:100%%;vertical-align:top}'
        '</style><main>%s</main>'
        % (
            DEF_BG, PAD_PX, DEF_BG, LINE_PX, LINE_PX, FONT_PX, LINE_PX,
            ''.join('<div>%s</div>' % r for r in rows_html),
        )
    )


def screenshot(page_html, out_png, work_dir):
    from PIL import Image

    chrome = next(
        (
            c
            for c in ('google-chrome', 'chromium', 'chromium-browser')
            if shutil.which(c)
        ),
        None,
    )
    if chrome is None:
        sys.exit('no Chrome or Chromium on PATH')

    html_path = os.path.join(work_dir, 'hero.html')
    raw_path = os.path.join(work_dir, 'hero-raw.png')
    with open(html_path, 'w') as handle:
        handle.write(page_html)

    width = COLS * FONT_PX * 6 // 10 + 2 * PAD_PX
    height = ROWS * LINE_PX + 2 * PAD_PX
    subprocess.run(
        [
            chrome,
            '--headless=new',
            '--disable-gpu',
            '--force-device-scale-factor=2',
            '--screenshot=' + raw_path,
            # headless Chrome reserves window chrome inside --window-size,
            # so ask for extra height and crop to the content below
            '--window-size=%d,%d' % (width, height + 120),
            '--hide-scrollbars',
            'file://' + html_path,
        ],
        check=True,
        capture_output=True,
    )
    Image.open(raw_path).crop((0, 0, width * 2, height * 2)).save(out_png)


def main():
    hero_dir = sys.argv[1]
    remax_bin = sys.argv[2]
    out_png = sys.argv[3]
    buf = capture(hero_dir, remax_bin)
    if len(buf) < 1000:
        sys.exit('remax produced no output — is the hero state seeded?')
    screenshot(to_html(buf), out_png, hero_dir)
    print('wrote ' + out_png)


if __name__ == '__main__':
    main()
