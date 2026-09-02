# A throwaway Claude Code configuration for the hero recording.
#
# The recording runs a real Claude Code session, which would otherwise
# arrive at a first-run wizard, a trust prompt and a permission prompt —
# none of which belong in a demonstration, and none of which can be
# answered by a script that is trying to show something else.
#
# So the session gets its own CLAUDE_CONFIG_DIR with those already
# settled, which also keeps the recording out of the real configuration:
# no history entries, no session files, no project list.

import json
import os
import shutil
import stat


def prepare(hero_dir, repo, skill_source):
    """Seed a config directory and answer for it. Returns its path."""
    cfg = os.path.join(hero_dir, 'claude')
    shutil.rmtree(cfg, ignore_errors=True)
    os.makedirs(cfg)
    # it holds a link to real credentials, so it is nobody else's business
    os.chmod(cfg, stat.S_IRWXU)

    with open(os.path.join(cfg, '.claude.json'), 'w', encoding='utf-8') as handle:
        json.dump(
            {
                # the first-run wizard, already answered
                'hasCompletedOnboarding': True,
                'theme': 'dark',
                'numStartups': 5,
                # and the "do you trust this folder" prompt, for the one
                # folder the recording ever opens
                'projects': {
                    repo: {
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

    # An allowlist rather than --dangerously-skip-permissions: the flag
    # opens with a full-screen warning that would be the first thing on
    # camera, and telling a reader to run that is not advice worth
    # printing next to a demonstration.
    with open(os.path.join(cfg, 'settings.json'), 'w', encoding='utf-8') as handle:
        json.dump(
            {
                'permissions': {
                    'defaultMode': 'acceptEdits',
                    'allow': ['Bash', 'Read', 'Edit', 'Write', 'Glob', 'Grep', 'Skill'],
                }
            },
            handle,
            indent=2,
        )

    # linked, never copied: the recording should not leave a second copy
    # of anyone's credentials lying around in a scratch directory
    real = os.path.expanduser('~/.claude/.credentials.json')
    if os.path.exists(real):
        os.symlink(real, os.path.join(cfg, '.credentials.json'))

    # the skill under test, where this session will look for it
    skills = os.path.join(cfg, 'skills')
    os.makedirs(skills, exist_ok=True)
    shutil.copytree(skill_source, os.path.join(skills, os.path.basename(skill_source)))

    return cfg
