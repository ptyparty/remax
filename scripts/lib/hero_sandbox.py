import json
import os
import shutil
import stat
import subprocess

import term_cast as tc

APPS = ('komux', 'quoll', 'kowari')

KEYRING_DENIES = (
    'Bash(secret-tool *)',
    'Bash(busctl --user call org.freedesktop.secrets *)',
    'Bash(gdbus call --session --dest org.freedesktop.secrets *)',
    'Bash(dbus-send --session --dest=org.freedesktop.secrets *)',
)

FISH_CONFIG = '''set -g fish_greeting
function fish_prompt
    set_color 7e9cd8; echo -n (prompt_pwd)' '; set_color 98bb6c; echo -n '❯ '; set_color normal
end
'''


def ptyparty_dir(repo):
    return os.environ.get('PTYPARTY_DIR') or os.path.dirname(repo)


def environment(root, ptyparty):
    sandbox = os.path.join(root, '.sandbox')
    env = tc.sandbox_env(sandbox, SHELL=shutil.which('fish') or '/bin/bash')
    bin_dir = os.path.join(sandbox, 'bin')
    os.makedirs(bin_dir, exist_ok=True)
    for app in APPS:
        cli = os.path.join(ptyparty, app, 'src', 'cli.ts')
        if not os.path.exists(cli):
            raise SystemExit('%s is not checked out at %s; set PTYPARTY_DIR' % (app, os.path.join(ptyparty, app)))
        path = os.path.join(bin_dir, app)
        with open(path, 'w') as handle:
            handle.write('#!/bin/sh\nexec bun %s "$@"\n' % cli)
        os.chmod(path, 0o755)
    env['PATH'] = bin_dir + os.pathsep + env['PATH']
    env['PTYPARTY_DIR'] = ptyparty
    fish = os.path.join(env['XDG_CONFIG_HOME'], 'fish')
    os.makedirs(fish, exist_ok=True)
    with open(os.path.join(fish, 'config.fish'), 'w') as handle:
        handle.write(FISH_CONFIG)
    return sandbox, env


def write_komux_config(env, text, **paths):
    folder = os.path.join(env['XDG_CONFIG_HOME'], 'komux')
    os.makedirs(folder, exist_ok=True)
    with open(os.path.join(folder, 'config.ts'), 'w') as handle:
        handle.write(text % {key: json.dumps(value) for key, value in paths.items()})


def project_from_fixture(fixture, target):
    shutil.copytree(fixture, target)
    for folder, _, files in os.walk(target):
        for name in files:
            if name.endswith('.fixture'):
                os.rename(os.path.join(folder, name), os.path.join(folder, name[:-len('.fixture')]))
    subprocess.run(['git', 'init', '-q', '-b', 'main', target], check=True)
    subprocess.run(['git', '-C', target, 'add', '-A'], check=True)
    subprocess.run(
        ['git', '-C', target, '-c', 'user.name=Demo', '-c', 'user.email=demo@example.com',
         'commit', '-q', '-m', 'Carts, coupons and money'],
        check=True,
    )
    return target


def claude_config(sandbox, project, skills, env, deny=()):
    cfg = os.path.join(sandbox, 'claude')
    shutil.rmtree(cfg, ignore_errors=True)
    os.makedirs(cfg)
    os.chmod(cfg, stat.S_IRWXU)
    with open(os.path.join(cfg, '.claude.json'), 'w') as handle:
        json.dump(
            {
                'hasCompletedOnboarding': True,
                'theme': 'dark',
                'numStartups': 5,
                'projects': {
                    project: {
                        'hasTrustDialogAccepted': True,
                        'hasCompletedProjectOnboarding': True,
                        'allowedTools': [],
                        'history': [],
                    }
                },
            },
            handle,
            indent=2,
        )
    with open(os.path.join(cfg, 'settings.json'), 'w') as handle:
        json.dump(
            {
                'permissions': {
                    'defaultMode': 'acceptEdits',
                    'allow': ['Bash', 'Read', 'Edit', 'Write', 'Glob', 'Grep', 'Skill'],
                    'deny': list(deny),
                },
                'spinnerTipsEnabled': False,
                'promptSuggestionEnabled': False,
                'env': {
                    'DISABLE_AUTOUPDATER': '1',
                    'CLAUDE_CODE_ENABLE_PROMPT_SUGGESTION': 'false',
                },
            },
            handle,
            indent=2,
        )
    real = os.path.expanduser('~/.claude/.credentials.json')
    if os.path.exists(real):
        os.symlink(real, os.path.join(cfg, '.credentials.json'))
    os.makedirs(os.path.join(cfg, 'skills'))
    for skill in skills:
        copy = os.path.join(cfg, 'skills', os.path.basename(skill))
        shutil.copytree(skill, copy)
    subprocess.run(
        ['komux', 'hooks', '--install'],
        env=dict(env, CLAUDE_CONFIG_DIR=cfg),
        check=True,
        capture_output=True,
    )
    decline_komux_setup(env)
    env['CLAUDE_CONFIG_DIR'] = cfg
    return cfg


def decline_komux_setup(env):
    komux = os.path.join(env['PTYPARTY_DIR'], 'komux')
    offered = ['skill:' + name for name in sorted(os.listdir(os.path.join(komux, 'skills')))]
    offered += ['rule:' + name[:-3] for name in sorted(os.listdir(os.path.join(komux, 'rules'))) if name.endswith('.md')]
    state = os.path.join(env['XDG_STATE_HOME'], 'komux')
    os.makedirs(state, exist_ok=True)
    with open(os.path.join(state, 'setup-declined'), 'w') as handle:
        handle.write('\n'.join(offered) + '\n')


def dock(text):
    return next((line for line in text.splitlines() if 'claude │' in line), '')


def record(argv, cols, rows, cwd, env, session, tour, out_base, max_hold_s=1.2):
    cast = tc.Cast(argv, cols, rows, cwd, env)
    try:
        finished = tour(cast)
    finally:
        cast.close()
        subprocess.run(['komux', 'kill', session], env=env, capture_output=True)
    if not finished:
        raise SystemExit('the tour did not finish; last screen:\n' + cast.text())
    frames = tc.write_gif(cast.frames, out_base + '.gif', max_hold_s=max_hold_s)
    print('wrote %s.gif (%.1f MB), %d frames' % (out_base, os.path.getsize(out_base + '.gif') / 1e6, frames))
