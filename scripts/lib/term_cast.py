import codecs
import fcntl
import os
import pty
import re
import signal
import struct
import sys
import termios
import time

import pyte
from PIL import Image, ImageDraw, ImageFont

FONT_PX, PAD_PX, LINE_PX = 15, 12, 19
DEF_FG, DEF_BG = '#dcd7ba', '#1f1f28'

NAMED = {
    'black': '#16161d', 'red': '#c34043', 'green': '#76946a',
    'brown': '#c0a36e', 'yellow': '#c0a36e', 'blue': '#7e9cd8',
    'magenta': '#957fb8', 'cyan': '#6a9589', 'white': '#c8c093',
    'brightblack': '#727169', 'brightred': '#e82424',
    'brightgreen': '#98bb6c', 'brightyellow': '#e6c384',
    'brightblue': '#7fb4ca', 'brightmagenta': '#938aa9',
    'brightcyan': '#7aa89f', 'brightwhite': '#dcd7ba',
}

FONTS = (
    '/usr/share/fonts/truetype/dejavu/DejaVuSansMono%s.ttf',
    '/usr/share/fonts/truetype/liberation/LiberationMono%s.ttf',
)

SAMPLE_S = 0.12

STRING_SEQUENCE = re.compile(r'\x1b[_P^X][^\x1b\x07]*(?:\x1b\\|\x07)')
STRING_START = re.compile(r'\x1b(?:[_P^X][^\x1b\x07]*)?$')


def load_fonts():
    for pattern in FONTS:
        plain = pattern % ''
        if os.path.exists(plain):
            faces = [pattern % suffix for suffix in ('', '-Bold', '-Oblique', '-BoldOblique')]
            return tuple(
                ImageFont.truetype(face if os.path.exists(face) else plain, FONT_PX)
                for face in faces
            )
    sys.exit('no monospace TTF found (looked for DejaVu and Liberation)')


def hex_rgb(text):
    return tuple(int(text[i:i + 2], 16) for i in (1, 3, 5))


def rgb(color, default):
    if color == 'default':
        return hex_rgb(default)
    if color in NAMED:
        return hex_rgb(NAMED[color])
    return hex_rgb('#' + color)


INHERITED_PREFIXES = ('QMUX', 'CLAUDE', 'ZELLIJ', 'TMUX', 'QUOLL', 'KOWARI', 'KITTY', 'XDG_', 'PG')


def sandbox_env(root, **extra):
    env = {k: v for k, v in os.environ.items() if not k.startswith(INHERITED_PREFIXES)}
    env.update(
        TERM='xterm-256color',
        COLORTERM='truecolor',
        HOME=os.path.join(root, 'home'),
        XDG_CONFIG_HOME=os.path.join(root, 'config'),
        XDG_STATE_HOME=os.path.join(root, 'state'),
        XDG_DATA_HOME=os.path.join(root, 'data'),
        XDG_CACHE_HOME=os.path.join(root, 'cache'),
        XDG_RUNTIME_DIR=os.path.join(root, 'runtime'),
    )
    env.update(extra)
    for key in ('HOME', 'XDG_CONFIG_HOME', 'XDG_STATE_HOME', 'XDG_DATA_HOME', 'XDG_CACHE_HOME', 'XDG_RUNTIME_DIR'):
        os.makedirs(env[key], mode=0o700, exist_ok=True)
    return env


class Cast:
    def __init__(self, argv, cols, rows, cwd, env):
        self.cols, self.rows = cols, rows
        self.frames = []
        self.pid, self.fd = pty.fork()
        if self.pid == 0:
            os.chdir(cwd)
            os.environ.clear()
            os.environ.update(env)
            os.execvp(argv[0], argv)
        os.set_blocking(self.fd, False)
        fcntl.ioctl(self.fd, termios.TIOCSWINSZ, struct.pack('HHHH', rows, cols, 0, 0))
        self.screen = pyte.Screen(cols, rows)
        self.stream = pyte.Stream(self.screen)
        self.decoder = codecs.getincrementaldecoder('utf-8')('replace')
        self.pending = ''

    def snapshot(self):
        rows = []
        for y in range(self.rows):
            row = []
            for x in range(self.cols):
                cell = self.screen.buffer[y][x]
                fg, bg = cell.fg, cell.bg
                if cell.reverse:
                    fg, bg = bg, fg
                row.append((cell.data or ' ', fg, bg, cell.bold, cell.italics, cell.underscore))
            rows.append(tuple(row))
        return tuple(rows)

    def pump(self):
        try:
            chunk = os.read(self.fd, 65536)
        except (BlockingIOError, OSError):
            time.sleep(0.004)
            return
        if chunk:
            text = STRING_SEQUENCE.sub('', self.pending + self.decoder.decode(chunk))
            unfinished = STRING_START.search(text)
            cut = unfinished.start() if unfinished else len(text)
            self.pending = text[cut:]
            self.stream.feed(text[:cut])

    def hold(self, seconds, record=True):
        end = time.time() + seconds
        nxt = time.time()
        play = SAMPLE_S
        while time.time() < end:
            self.pump()
            if record and time.time() >= nxt:
                self.frames.append((self.snapshot(), play))
                nxt += play

    def lapse_until(self, done, budget, rate=2.0, play=0.12):
        end = time.time() + budget
        nxt = time.time()
        while time.time() < end:
            self.pump()
            if time.time() >= nxt:
                self.frames.append((self.snapshot(), play))
                nxt += rate
                if done(self.text()):
                    return True
        return False

    def text(self):
        return '\n'.join(self.screen.display)

    def wait_for(self, needle, budget=30, record=False):
        end = time.time() + budget
        while time.time() < end:
            self.hold(0.1, record=record)
            if needle in self.text():
                return
        sys.exit('never saw %r; last screen:\n%s' % (needle, self.text()))

    def send(self, data):
        os.write(self.fd, data.encode() if isinstance(data, str) else data)

    def keys(self, text, per_key=0.07):
        last = time.time()
        for ch in text:
            self.send(ch)
            self.hold(per_key, record=False)
            if time.time() - last >= SAMPLE_S:
                self.frames.append((self.snapshot(), time.time() - last))
                last = time.time()
        self.frames.append((self.snapshot(), max(0.04, time.time() - last)))

    def press(self, *seqs, gap=0.25):
        for seq in seqs:
            self.send(seq)
            self.hold(gap)

    def close(self):
        try:
            os.kill(self.pid, signal.SIGKILL)
            os.waitpid(self.pid, 0)
        except (ProcessLookupError, ChildProcessError):
            pass
        os.close(self.fd)


def render(snapshot, fonts):
    cell_w = round(fonts[0].getlength('M'))
    rows, cols = len(snapshot), len(snapshot[0])
    image = Image.new('RGB', (cols * cell_w + 2 * PAD_PX, rows * LINE_PX + 2 * PAD_PX), hex_rgb(DEF_BG))
    draw = ImageDraw.Draw(image)
    for y, row in enumerate(snapshot):
        top = PAD_PX + y * LINE_PX
        start, run_bg = 0, row[0][2]
        for x in range(cols + 1):
            here = row[x][2] if x < cols else None
            if here != run_bg or x == cols:
                if run_bg != 'default':
                    draw.rectangle(
                        [PAD_PX + start * cell_w, top, PAD_PX + x * cell_w - 1, top + LINE_PX - 1],
                        fill=rgb(run_bg, DEF_BG),
                    )
                start, run_bg = x, here
        for x, (char, fg, _, is_bold, is_italic, is_underlined) in enumerate(row):
            left, colour = PAD_PX + x * cell_w, rgb(fg, DEF_FG)
            if is_underlined and (char != ' ' or underlined_between(row, x)):
                draw.line([left, top + LINE_PX - 2, left + cell_w - 1, top + LINE_PX - 2], fill=colour)
            if char == ' ' or draw_cell_art(draw, char, left, top, cell_w, colour):
                continue
            if is_emoji(char):
                paste_emoji(image, char, left, top, cell_w)
                continue
            draw.text((left, top), char, font=fonts[is_bold + 2 * is_italic], fill=colour)
    return image


EMOJI_FONT = '/usr/share/fonts/truetype/noto/NotoColorEmoji.ttf'

NARROW_EMOJI = set('⏳⌛⏰⏸⏹⏺⚡⚠⭐❌❓❗')

_emoji_cache = {}


def is_emoji(char):
    return ord(char[0]) >= 0x1F000 or char[0] in NARROW_EMOJI


def paste_emoji(image, char, left, top, cell_w):
    if not os.path.exists(EMOJI_FONT):
        return
    size = (2 * cell_w, LINE_PX)
    if char not in _emoji_cache:
        font = ImageFont.truetype(EMOJI_FONT, 109)
        glyph = Image.new('RGBA', (136, 128), (0, 0, 0, 0))
        ImageDraw.Draw(glyph).text((0, 0), char, font=font, embedded_color=True)
        box = glyph.getbbox()
        if box:
            glyph = glyph.crop(box)
        side = LINE_PX - 3
        glyph.thumbnail((side, side), Image.LANCZOS)
        _emoji_cache[char] = glyph
    glyph = _emoji_cache[char]
    x = left + (size[0] - glyph.width) // 2
    y = top + (size[1] - glyph.height) // 2
    image.paste(glyph, (x, y), glyph)


def underlined_between(row, x):
    def glyph(cell):
        return cell[0] != ' ' and cell[5]

    left = any(glyph(cell) for cell in row[max(0, x - 2):x])
    right = any(glyph(cell) for cell in row[x + 1:x + 3])
    return left and right


LINES = {
    '─': 'lr', '━': 'lr', '│': 'ud', '┃': 'ud',
    '┌': 'rd', '┐': 'ld', '└': 'ru', '┘': 'lu',
    '╭': 'rd', '╮': 'ld', '╰': 'ru', '╯': 'lu',
    '┏': 'rd', '┓': 'ld', '┗': 'ru', '┛': 'lu',
    '├': 'udr', '┤': 'udl', '┬': 'lrd', '┴': 'lru', '┼': 'udlr',
    '┣': 'udr', '┫': 'udl', '┳': 'lrd', '┻': 'lru', '╋': 'udlr',
    '╴': 'l', '╶': 'r', '╵': 'u', '╷': 'd',
}

ROUNDED = {'╭', '╮', '╰', '╯'}

HEAVY = set('━┃┏┓┗┛┣┫┳┻╋')

BLOCKS = {
    '█': (0, 0, 8, 8), '▀': (0, 0, 8, 4), '▄': (0, 4, 8, 8),
    '▌': (0, 0, 4, 8), '▐': (4, 0, 8, 8),
    '▁': (0, 7, 8, 8), '▂': (0, 6, 8, 8), '▃': (0, 5, 8, 8),
    '▅': (0, 3, 8, 8), '▆': (0, 2, 8, 8), '▇': (0, 1, 8, 8),
    '▏': (0, 0, 1, 8), '▎': (0, 0, 2, 8), '▍': (0, 0, 3, 8),
    '▋': (0, 0, 5, 8), '▊': (0, 0, 6, 8), '▉': (0, 0, 7, 8),
}


def draw_cell_art(draw, char, left, top, cell_w, fill):
    if char in BLOCKS:
        x0, y0, x1, y1 = BLOCKS[char]
        draw.rectangle(
            [
                left + cell_w * x0 // 8,
                top + LINE_PX * y0 // 8,
                left + cell_w * x1 // 8 - 1,
                top + LINE_PX * y1 // 8 - 1,
            ],
            fill=fill,
        )
        return True
    arms = LINES.get(char)
    if arms is None:
        return False
    width = 2 if char in HEAVY else 1
    cx, cy = left + cell_w // 2, top + LINE_PX // 2
    right, bottom = left + cell_w - 1, top + LINE_PX - 1
    if char in ROUNDED:
        r = min(cell_w, LINE_PX) // 2
        bx = cx if 'r' in arms else cx - 2 * r
        by = cy if 'd' in arms else cy - 2 * r
        start = {'rd': 180, 'ld': 270, 'lu': 0, 'ru': 90}[arms]
        draw.arc([bx, by, bx + 2 * r, by + 2 * r], start, start + 90, fill=fill, width=width)
        if 'r' in arms:
            draw.line([cx + r, cy, right, cy], fill=fill, width=width)
        else:
            draw.line([left, cy, cx - r, cy], fill=fill, width=width)
        if 'd' in arms:
            draw.line([cx, cy + r, cx, bottom], fill=fill, width=width)
        else:
            draw.line([cx, top, cx, cy - r], fill=fill, width=width)
        return True
    if 'l' in arms:
        draw.line([left, cy, cx, cy], fill=fill, width=width)
    if 'r' in arms:
        draw.line([cx, cy, right, cy], fill=fill, width=width)
    if 'u' in arms:
        draw.line([cx, top, cx, cy], fill=fill, width=width)
    if 'd' in arms:
        draw.line([cx, cy, cx, bottom], fill=fill, width=width)
    return True


def collapse(samples):
    runs = []
    for snapshot, play in samples:
        if runs and runs[-1][0] == snapshot:
            runs[-1][1] += play
        else:
            runs.append([snapshot, play])
    return runs


def write_png(snapshot, out_path):
    render(snapshot, load_fonts()).save(out_path, optimize=True)


def write_gif(samples, out_path, max_hold_s=1.6):
    fonts = load_fonts()
    runs = collapse(samples)
    while runs and all(cell[0] == ' ' for row in runs[0][0] for cell in row):
        runs.pop(0)
    images = [render(s, fonts) for s, _ in runs]
    durations = [max(40, min(int(max_hold_s * 1000), int(d * 1000))) for _, d in runs]
    durations[-1] = 3000
    sample = images[:: max(1, len(images) // 12)][:12]
    montage = Image.new('RGB', (images[0].width, images[0].height * len(sample)))
    for n, frame in enumerate(sample):
        montage.paste(frame, (0, n * images[0].height))
    master = montage.quantize(colors=255, method=Image.MEDIANCUT)
    paletted = [im.quantize(palette=master, dither=Image.Dither.NONE) for im in images]
    paletted[0].save(
        out_path,
        save_all=True,
        append_images=paletted[1:],
        duration=durations,
        loop=0,
        optimize=True,
    )
    return len(images)
