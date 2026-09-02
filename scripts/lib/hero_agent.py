# The agent side of the hero recording, run for real rather than mimed.
#
# It waits on the reviewer with `remax list --wait`, which blocks until
# the punch list changes, then reads the flag, edits the file, and
# answers. Nothing here knows the recording exists — this is the loop the
# recording is meant to show, and it is the same shape an agent harness
# would run.

import json
import os
import subprocess
import sys
import time

NOTE_MARKER = 'capped ms'
FIX_FROM = '    map (capped . grow) [0 .. retryLimit policy - 1]'
FIX_TO = '    map (max 1 . capped . grow) [0 .. retryLimit policy - 1]'
ANSWER = 'floored at 1ms in the pipeline, so a zero cap still retries'


def main():
    remax, repo = sys.argv[1], sys.argv[2]

    # Blocks here until the reviewer flags something — and then again,
    # because the first thing to change is the flag itself and the note
    # is still being typed. An agent that acts on the bare flag answers
    # before it has been told what is wrong, and its reply lands ahead of
    # the note in the thread. So: wait until there is something to read.
    flagged = None
    while flagged is None:
        listing = subprocess.run(
            [remax, '-C', repo, 'list', '--wait', '--json'],
            capture_output=True,
            text=True,
            check=True,
        )
        items = json.loads(listing.stdout)['items']
        flagged = next(
            (
                i
                for i in items
                if NOTE_MARKER in i['line']
                and i['file'].endswith('Retry.hs')
                and i['note']
            ),
            None,
        )

    path = os.path.join(repo, 'src/Retry.hs')
    with open(path, encoding='utf-8') as handle:
        source = handle.read()
    # a different line from the flagged one, deliberately: a verdict is
    # keyed by the content of its own line, so rewriting the line it was
    # asked about would take the conversation off it
    with open(path, 'w', encoding='utf-8') as handle:
        handle.write(source.replace(FIX_FROM, FIX_TO))

    # a beat, so the edit and the answer read as two things happening
    # rather than one
    time.sleep(1.4)
    subprocess.run(
        [remax, '-C', repo, 'reply', flagged['key'], ANSWER],
        capture_output=True,
        check=True,
    )


if __name__ == '__main__':
    main()
