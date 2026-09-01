# Records the hero animation: drives remax in a pty over the seeded hero
# state, samples the emulated screen as it goes, and writes the frames out
# as an animated GIF.
#
# Driven by scripts/hero-cast; expects the hero state to already exist
# (scripts/hero --no-launch builds it).
#
# Rendered with Pillow rather than through a browser the way the still
# was: a screenshot per frame is a Chrome launch per frame, and drawing
# the cells directly is both faster and exact about the grid.

import fcntl
import json
import os
import pty
import signal
import struct
import subprocess
import sys
import termios
import time

import pyte
from PIL import Image, ImageDraw, ImageFont

COLS, ROWS = 132, 34
FONT_PX, PAD_PX = 15, 12
LINE_PX = 19

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

FONTS = (
    '/usr/share/fonts/truetype/dejavu/DejaVuSansMono%s.ttf',
    '/usr/share/fonts/truetype/liberation/LiberationMono%s.ttf',
)

# How often the screen is sampled. Identical consecutive samples are
# collapsed into one frame that simply lasts longer, so a still stretch
# costs one frame rather than a dozen.
SAMPLE_S = 0.09


def load_fonts():
    for pattern in FONTS:
        plain, bold = pattern % '', pattern % '-Bold'
        if os.path.exists(plain):
            return (
                ImageFont.truetype(plain, FONT_PX),
                ImageFont.truetype(bold if os.path.exists(bold) else plain, FONT_PX),
            )
    sys.exit('no monospace TTF found (looked for DejaVu and Liberation)')


def hex_rgb(text):
    return tuple(int(text[i:i + 2], 16) for i in (1, 3, 5))


# pyte reports a colour as 'default', as an ANSI name, or as bare hex
def rgb(color, default):
    if color == 'default':
        return hex_rgb(default)
    if color in NAMED:
        return hex_rgb(NAMED[color])
    return hex_rgb('#' + color)


class Cast:
    """A live remax under a pty, sampled as it is driven."""

    def __init__(self, hero_dir, remax_bin):
        self.hero_dir = hero_dir
        self.repo = os.path.join(hero_dir, 'repo')
        self.env = dict(
            os.environ,
            TERM='xterm-256color',
            COLORTERM='truecolor',
            XDG_STATE_HOME=os.path.join(hero_dir, 'state'),
            XDG_CONFIG_HOME=os.path.join(hero_dir, 'config'),
        )
        self.remax_bin = remax_bin
        self.frames = []
        self.pid, self.fd = pty.fork()
        if self.pid == 0:
            os.chdir(self.repo)
            os.environ.update(self.env)
            os.execvp(remax_bin, [remax_bin])
        os.set_blocking(self.fd, False)
        fcntl.ioctl(
            self.fd, termios.TIOCSWINSZ, struct.pack('HHHH', ROWS, COLS, 0, 0)
        )
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.Stream(self.screen)

    def snapshot(self):
        rows = []
        for y in range(ROWS):
            row = []
            for x in range(COLS):
                cell = self.screen.buffer[y][x]
                fg, bg = cell.fg, cell.bg
                if cell.reverse:
                    fg, bg = bg, fg
                row.append((cell.data or ' ', fg, bg, cell.bold))
            rows.append(tuple(row))
        return tuple(rows)

    def hold(self, seconds):
        """Read the pty for a while, sampling the screen as it changes."""
        end = time.time() + seconds
        nxt = time.time()
        while time.time() < end:
            try:
                chunk = os.read(self.fd, 65536)
                if chunk:
                    self.stream.feed(chunk.decode('utf8', 'replace'))
            except (BlockingIOError, OSError):
                time.sleep(0.004)
            if time.time() >= nxt:
                self.frames.append(self.snapshot())
                nxt += SAMPLE_S

    def keys(self, text, per_key=0.07):
        for ch in text:
            os.write(self.fd, ch.encode())
            self.hold(per_key)

    def shell(self, args):
        """What an agent does from outside the session, while it watches."""
        return subprocess.run(
            args, cwd=self.repo, env=self.env, capture_output=True, text=True
        )

    def close(self):
        os.kill(self.pid, signal.SIGKILL)
        try:
            os.waitpid(self.pid, 0)
        except ChildProcessError:
            pass
        os.close(self.fd)


def render(snapshot, fonts, cell_w):
    plain, bold = fonts
    width = COLS * cell_w + 2 * PAD_PX
    height = ROWS * LINE_PX + 2 * PAD_PX
    image = Image.new('RGB', (width, height), hex_rgb(DEF_BG))
    draw = ImageDraw.Draw(image)
    for y, row in enumerate(snapshot):
        top = PAD_PX + y * LINE_PX
        # backgrounds first, in runs, so a cell's block glyph is never
        # clipped by the next cell's background painted over it
        start, run_bg = 0, row[0][2]
        for x in range(COLS + 1):
            here = row[x][2] if x < COLS else None
            if here != run_bg or x == COLS:
                if run_bg != 'default':
                    draw.rectangle(
                        [
                            PAD_PX + start * cell_w,
                            top,
                            PAD_PX + x * cell_w - 1,
                            top + LINE_PX - 1,
                        ],
                        fill=rgb(run_bg, DEF_BG),
                    )
                start, run_bg = x, here
        for x, (char, fg, _, is_bold) in enumerate(row):
            if char == ' ':
                continue
            draw.text(
                (PAD_PX + x * cell_w, top),
                char,
                font=bold if is_bold else plain,
                fill=rgb(fg, DEF_FG),
            )
    return image


def write_gif(frames, out_path):
    """One shared palette for the whole animation, so colours do not
    shimmer from frame to frame the way per-frame quantisation makes
    them."""
    images, durations = [], []
    for snapshot, seconds in frames:
        images.append(snapshot)
        durations.append(max(40, int(seconds * 1000)))

    # The palette is taken from a spread of frames rather than the first
    # one. The first frame is the terminal before remax has drawn a
    # thing, so a palette built from it holds one colour and quantising
    # the rest against it turns the whole animation black.
    sample = images[:: max(1, len(images) // 12)][:12]
    montage = Image.new('RGB', (images[0].width, images[0].height * len(sample)))
    for n, frame in enumerate(sample):
        montage.paste(frame, (0, n * images[0].height))
    master = montage.quantize(colors=255, method=Image.MEDIANCUT)
    paletted = [
        im.quantize(palette=master, dither=Image.Dither.NONE) for im in images
    ]
    paletted[0].save(
        out_path,
        save_all=True,
        append_images=paletted[1:],
        duration=durations,
        loop=0,
        optimize=True,
    )


def collapse(snapshots):
    """Identical consecutive samples become one frame that lasts longer."""
    runs = []
    for snapshot in snapshots:
        if runs and runs[-1][0] == snapshot:
            runs[-1][1] += SAMPLE_S
        else:
            runs.append([snapshot, SAMPLE_S])
    return runs


def tour(cast):
    """The loop remax exists for: read a change, ask for something, and
    watch an agent answer in the file while the session is still open."""
    cast.hold(2.2)

    # accept the first hunk: the meter fills and the review moves itself
    # on to the next one, which is where the delay calculation changed
    # and where the words that differ inside a line carry their own mark
    cast.keys('x')
    cast.hold(0.8)
    cast.keys('\r')
    cast.hold(2.2)

    # flag the line the calculation starts on, and say what is missing
    cast.keys('x')
    cast.hold(0.8)
    cast.keys('!')
    cast.hold(1.2)
    cast.keys('a zero retryCapMs would disable retries — floor it', per_key=0.045)
    cast.hold(0.9)
    cast.keys('\033')
    cast.hold(0.5)
    cast.keys('\r')
    cast.hold(2.0)

    # what the agent does, from outside the session, while it watches
    listing = cast.shell([cast.remax_bin, 'list', '--json'])
    items = json.loads(listing.stdout)['items']
    flagged = next(
        (i for i in items if i['file'] == 'src/Retry.hs' and 'grow' in i['line']),
        None,
    )
    if flagged is None:
        sys.exit('the flag did not land — remax list saw no grow line')

    path = os.path.join(cast.repo, 'src/Retry.hs')
    with open(path, encoding='utf-8') as handle:
        source = handle.read()
    source = source.replace(
        '    capped ms = min (retryCapMs policy) ms',
        '    capped ms = max 1 (min (retryCapMs policy) ms)',
    )
    with open(path, 'w', encoding='utf-8') as handle:
        handle.write(source)
    # the edit lands on the line below the flagged one deliberately: a
    # verdict is keyed by the content of its own line, so an agent that
    # rewrites the line it was asked about takes the conversation off it
    #
    # the watcher notices the file and the store, so both the rewritten
    # line and the answer arrive without a keystroke
    cast.hold(1.8)
    cast.shell(
        [
            cast.remax_bin,
            'reply',
            flagged['key'],
            'floored at 1ms, so a zero cap still retries',
        ]
    )
    # rest on the answer: it is the whole point of the loop, and
    # accepting from here would scroll it off the screen
    cast.hold(4.5)


def main():
    hero_dir, remax_bin, out_gif = sys.argv[1], sys.argv[2], sys.argv[3]
    fonts = load_fonts()
    cell_w = round(fonts[0].getlength('M'))

    cast = Cast(hero_dir, remax_bin)
    try:
        tour(cast)
    finally:
        cast.close()

    if len(cast.frames) < 10:
        sys.exit('remax produced almost no output — is the hero state seeded?')

    runs = collapse(cast.frames)
    # the terminal before remax has drawn anything is not worth a frame
    while runs and all(cell[0] == ' ' for row in runs[0][0] for cell in row):
        runs.pop(0)
    write_gif([(render(s, fonts, cell_w), d) for s, d in runs], out_gif)
    size = os.path.getsize(out_gif)
    print(
        'wrote %s — %d frames, %.1f MB' % (out_gif, len(runs), size / 1e6)
    )


if __name__ == '__main__':
    main()
