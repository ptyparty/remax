import os
import re
import shutil
import subprocess
import sys

import hero_sandbox as hs

HERO_DIR = os.environ.get('HERO_DIR', '/tmp/remax-hero')
REPO = os.path.join(HERO_DIR, 'repo')
SESSION = 'hero'
COLS, ROWS = 200, 50
ALT_O, ESC = '\x1bo', '\x1b'
NOTE = 'a zero retryCapMs zeroes every delay — floor capMs where it is bound'

QMUX_CONFIG = '''export default {
  defaultCwd: %(repo)s,
  untrackedCommands: ["claude", "remax", "hx", "qmux", "quoll"],
  tabs: { "retry-queue": { cwd: %(repo)s, layout: "review" } },
  startup: ["retry-queue"],
  layouts: {
    review: {
      row: [
        { command: ["qmux", "claude", "--mode", "auto"], name: "claude", focus: true },
        { command: ["remax"], name: "remax" },
      ],
    },
  },
};
'''


def prepare(repo):
    subprocess.run([os.path.join(repo, 'scripts', 'hero'), '--no-launch'], check=True, capture_output=True)
    sandbox, env = hs.environment(HERO_DIR, hs.ptyparty_dir(repo))
    env['XDG_STATE_HOME'] = os.path.join(HERO_DIR, 'state')
    env['XDG_CONFIG_HOME'] = os.path.join(HERO_DIR, 'config')
    shutil.copytree(os.path.join(sandbox, 'config', 'fish'), os.path.join(env['XDG_CONFIG_HOME'], 'fish'))
    remax_bin = os.environ.get('REMAX_BIN')
    if remax_bin:
        shim = os.path.join(sandbox, 'bin', 'remax')
        with open(shim, 'w') as handle:
            handle.write('#!/bin/sh\nexec %s "$@"\n' % os.path.abspath(remax_bin))
        os.chmod(shim, 0o755)
    hs.write_qmux_config(env, QMUX_CONFIG, repo=REPO)
    hs.claude_config(sandbox, REPO, [os.path.join(repo, 'skills', 'remax-review')], env)
    return env


def remax_pane(text):
    return '\n'.join(line[COLS // 2:] for line in text.splitlines())


def tour(cast):
    cast.wait_for('Message Claude', budget=40)
    cast.hold(1.5)
    cast.keys('/remax-review watch', per_key=0.06)
    cast.press('\r', gap=0.8)
    if not cast.lapse_until(lambda t: '🚀 done' in t, budget=300):
        return False
    cast.hold(2)
    cast.press(ALT_O, gap=0.8)
    cast.keys('x', per_key=0.8)
    cast.press('\r', gap=2)
    cast.keys('jj', per_key=0.4)
    cast.keys('x', per_key=0.8)
    cast.keys('!', per_key=1.0)
    cast.keys(NOTE, per_key=0.03)
    cast.press(ESC, gap=0.8)
    cast.press('\r', gap=0.8)
    if not cast.lapse_until(lambda t: re.search(r'claude [╭┌]', remax_pane(t)) is not None, budget=300):
        return False
    cast.hold(2)
    cast.lapse_until(lambda t: '🚀 done' in t, budget=180)
    cast.hold(5)
    return True


def main():
    out_base = sys.argv[1]
    repo = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..'))
    env = prepare(repo)
    hs.record(['qmux', 'attach', SESSION], COLS, ROWS, REPO, env, SESSION, tour, out_base)


if __name__ == '__main__':
    main()
