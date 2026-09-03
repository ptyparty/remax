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

COLS, ROWS = 132, 44
REMAX_ROWS = 27  # the bottom pane; Claude gets what is left
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
SAMPLE_S = 0.12

# and while waiting on a model: sampled this often, played back this
# fast, so the waiting reads as time passing rather than being sat
# through. Sampled coarsely on purpose — the presence mark animates, so
# no two samples are ever identical and none of them collapse, and a
# finer grain buys nothing but half a minute of watching a spinner
LAPSE_S = 3.0
LAPSE_PLAY_S = 0.11


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


SOCKET = 'remax-hero'


class Cast:
    """Two panes under one pty — Claude Code above, remax below — sampled
    as they are driven.

    tmux does the splitting so both are one terminal to record, on its own
    socket and with no configuration file, so it cannot disturb or be
    disturbed by whatever tmux the reader happens to be running. Panes are
    driven by name rather than by whatever has focus."""

    def __init__(self, hero_dir, remax_bin, claude_cfg):
        self.hero_dir = hero_dir
        self.repo = os.path.join(hero_dir, 'repo')
        # the parent session's own variables would be inherited by the
        # Claude Code the recording starts, which then thinks it is a
        # child of it
        parent = {
            k: v
            for k, v in os.environ.items()
            if not k.startswith('CLAUDE') and k != 'CLAUDECODE'
        }
        self.env = dict(
            parent,
            TERM='xterm-256color',
            COLORTERM='truecolor',
            XDG_STATE_HOME=os.path.join(hero_dir, 'state'),
            XDG_CONFIG_HOME=os.path.join(hero_dir, 'config'),
            CLAUDE_CONFIG_DIR=claude_cfg,
        )
        self.remax_bin = remax_bin
        self.frames = []

        self.tmux('kill-server', check=False)
        self.tmux(
            'new-session', '-d', '-s', 'hero',
            '-x', str(COLS), '-y', str(ROWS), '-c', self.repo, 'claude',
        )
        self.tmux('set-option', '-g', 'status', 'off')
        # otherwise Claude Code notices they are off and says so, on camera
        self.tmux('set-option', '-g', 'focus-events', 'on')
        self.tmux(
            'split-window', '-v', '-l', str(REMAX_ROWS),
            '-t', 'hero:0.0', '-c', self.repo, remax_bin,
        )

        self.pid, self.fd = pty.fork()
        if self.pid == 0:
            os.chdir(self.repo)
            os.environ.clear()
            os.environ.update(self.env)
            os.execvp('tmux', ['tmux', '-L', SOCKET, '-f', '/dev/null', 'attach', '-t', 'hero'])
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

    def hold(self, seconds, lapse=False):
        """Read the pty for a while, sampling the screen as it changes.

        `lapse` is for waiting on something slow — a model thinking. It
        samples rarely and plays those samples back quickly, so a minute
        of spinner becomes a few seconds of one. Without it a spinner
        defeats the frame collapsing entirely: every sample differs, so
        nothing merges, and the recording is as long as the wait."""
        rate, play = (LAPSE_S, LAPSE_PLAY_S) if lapse else (SAMPLE_S, SAMPLE_S)
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
                self.frames.append((self.snapshot(), play))
                nxt += rate

    def tmux(self, *args, check=True):
        return subprocess.run(
            ['tmux', '-L', SOCKET, '-f', '/dev/null'] + list(args),
            env=self.env,
            capture_output=True,
            check=check,
        )

    CLAUDE, REMAX = 'hero:0.0', 'hero:0.1'

    def keys(self, pane, text, per_key=0.07):
        """Literal characters, one at a time so it reads as typing."""
        for ch in text:
            self.tmux('send-keys', '-t', pane, '-l', ch)
            self.hold(per_key)

    def press(self, pane, *names):
        """Named keys — Enter, Escape — which send-keys spells out."""
        for name in names:
            self.tmux('send-keys', '-t', pane, name)
            self.hold(0.25)

    def text(self):
        return '\n'.join(
            ''.join(cell[0] for cell in row) for row in self.snapshot()
        )

    def upper_text(self):
        """Just the pane above remax — the one Claude Code is in."""
        rows = self.snapshot()[: ROWS - REMAX_ROWS]
        return '\n'.join(''.join(cell[0] for cell in row) for row in rows)

    def wait_ready(self, budget=150):
        """Hold until the agent pane will actually take a command.

        Typing at a pane that is still starting throws the keystrokes
        away, and what is lost is the command this whole recording is
        about — which is how it broke: a new version put a prompt in the
        way, the pane sat on it, and the tour typed into a dialog.

        Waiting for a settled screen is not enough either, since a pane
        that has not drawn anything yet is perfectly settled."""
        end = time.time() + budget
        answered = False
        seen = ''
        while time.time() < end:
            self.hold(0.4)
            seen = self.upper_text()
            if 'accept edits on' in seen:
                return
            # a version of Claude Code that asks about the settings it
            # was handed; the answer is yes, and it is already selected
            if not answered and 'trust these settings' in seen:
                self.press(self.CLAUDE, 'Enter')
                answered = True
        sys.exit('the agent pane never became usable; last saw:\n' + seen)

    def shell(self, args):
        """What an agent does from outside the session, while it watches."""
        return subprocess.run(
            args, cwd=self.repo, env=self.env, capture_output=True, text=True
        )

    def close(self):
        self.tmux('kill-server', check=False)
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
    # A still stretch is capped rather than played out: waiting for a
    # model to answer is most of the elapsed time and none of the
    # interest. Nothing is reordered or dropped — a pause just stops
    # being real-time.
    images, durations = [], []
    for snapshot, seconds in frames:
        images.append(snapshot)
        durations.append(max(40, min(1600, int(seconds * 1000))))

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


def collapse(samples):
    """Identical consecutive samples become one frame that lasts longer."""
    runs = []
    for snapshot, play in samples:
        if runs and runs[-1][0] == snapshot:
            runs[-1][1] += play
        else:
            runs.append([snapshot, play])
    return runs


def tour(cast):
    """The loop remax exists for, with both halves of it on screen: a
    reviewer reads a change and asks for something, and Claude Code —
    started here, watching from the pane above — answers in the file
    while the session is still open."""
    # clear the startup banner out of the Claude pane: it carries a
    # version, a plan name and whatever notice that account happens to be
    # showing, none of which is what this is about
    cast.tmux('send-keys', '-t', cast.CLAUDE, 'C-l')
    cast.hold(3.0)

    # the reviewer accepts a hunk: the meter fills and the review moves
    # on to where the delay calculation changed, and where the words that
    # differ inside a line carry their own mark
    cast.keys(cast.REMAX, 'x')
    cast.hold(0.8)
    cast.press(cast.REMAX, 'Enter')
    cast.hold(2.2)

    # and starts an agent watching. `watch` is what tells it to stay:
    # without it the skill answers what is outstanding and stops.
    #
    # HERO_SKIP_AGENT leaves the prompt untyped, for checking the layout
    # and the remax half without spending a model call on it.
    if not os.environ.get('HERO_SKIP_AGENT'):
        cast.wait_ready()
        cast.keys(cast.CLAUDE, '/remax-review watch', per_key=0.05)
        cast.hold(1.2)
        cast.press(cast.CLAUDE, 'Enter')
    cast.hold(float(os.environ.get('HERO_SETTLE', '55')), lapse=True)

    # Flag the capped line and ask for the fix on the line above it.
    #
    # Two things about which line. It is purely added, so it carries one
    # verdict key — flagging a *changed* line covers both its removal and
    # its addition and renders the note under each. And the fix belongs
    # somewhere else, because a verdict is keyed by the content of its own
    # line: an agent that rewrites the line it was asked about re-keys it,
    # and the conversation comes off the line with the answer still in it.
    cast.keys(cast.REMAX, 'jj')
    cast.hold(0.7)
    cast.keys(cast.REMAX, 'x')
    cast.hold(0.8)
    cast.keys(cast.REMAX, '!')
    cast.hold(1.2)
    cast.keys(
        cast.REMAX,
        'a zero retryCapMs zeroes every delay — floor capMs where it is bound',
        per_key=0.04,
    )
    cast.hold(0.9)
    cast.press(cast.REMAX, 'Escape')
    cast.hold(0.5)
    cast.press(cast.REMAX, 'Enter')

    # from here nothing is typed at remax. The wait above returns, and
    # the rewritten line and the answer arrive because the session
    # watches the file and the store.
    cast.hold(float(os.environ.get('HERO_REACT', '75')), lapse=True)


def main():
    hero_dir, remax_bin, out_gif = sys.argv[1], sys.argv[2], sys.argv[3]
    fonts = load_fonts()
    cell_w = round(fonts[0].getlength('M'))

    here = os.path.dirname(os.path.abspath(__file__))
    sys.path.insert(0, here)
    import hero_claude

    cfg = hero_claude.prepare(
        hero_dir,
        os.path.join(hero_dir, 'repo'),
        os.path.join(os.path.dirname(here), '..', 'skills', 'remax-review'),
    )
    cast = Cast(hero_dir, remax_bin, cfg)
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
