# Staying and reacting

`list` answers with whatever is recorded right now. `--wait` prints
nothing until the punch list *changes*, then prints it and exits. Run it
as a **background task** and your harness wakes you when the reviewer
does something — you react to a flag the way they react to your edits,
and neither of you has to prompt the other.

Start one as soon as the one-off pass is done, and **always pass
`--awaiting agent` and `--state`**:

```sh
SEEN="/tmp/remax-seen$(printf %s "$REPO" | tr / -)"
FEEDBACK="/tmp/remax-feedback$(printf %s "$REPO" | tr / -)"
remax -C "$REPO" list --wait --awaiting agent --state "$SEEN" \
  > "$FEEDBACK.part" && mv "$FEEDBACK.part" "$FEEDBACK"
```

The rename keeps `$FEEDBACK` holding the last complete listing while
the next wait runs. Reading it mid-wait gives you what you last saw
rather than an empty file.

Only the reviewer wakes this wait: a note, a reply in a thread, a
GitHub post, a `:tour ask`, or a move of the review into another
checkout. `--awaiting agent` is what keeps your own work out. Your reply
hands its conversation to the reviewer, which drops it from the
listing, and the wait counts only a new or changed conversation as
news. So you never need to wait twice to get past your own work.
Without `--awaiting agent` every change counts, and each of your
replies wakes the wait at once. When it wakes, act on **everything**
in the listing, `tour` and `moved` lines included — a request you skip
is already recorded as seen, and no later wait will show it again. The
listing marks each conversation `new` or `changed` against the state
file, so you can tell what woke you without comparing listings
yourself.

The state file is where remax keeps a fingerprint of the listing you were
last shown. Without it a wait measures change from the moment it starts,
which is not the moment you last looked — everything that happened while
you were reading, thinking and writing has been absorbed into the new
baseline, and the wait sits through it. With it, a change that landed
while you were busy is still there waiting for you.

**Name both files after the review**, as above, rather than picking one
fixed path. The fingerprint is of a particular repository, and it has to
survive from one of your waits to the next — so it cannot be per-process
— which means a fixed path is shared with every other agent on the
machine watching some other review. remax will not act on a fingerprint
recorded for somewhere else, so nothing wakes you spuriously; but it has
nothing to measure from either, and falls back to measuring from now.
That is exactly the case `--state` exists to prevent: whatever the
reviewer did while you were busy is absorbed into the baseline and you
sit through it. The output file wants its own name for the plainer
reason that two agents writing one path overwrite each other's feedback.

When it completes you are woken with it. Read the file, act on it, and
**start another one** — a wait is a single shot, and while none is
running you are back to being prompted. Keep exactly one in flight;
several waiting on the same repository all wake at once and you answer
the same feedback that many times.

It blocks from the moment it starts, so it reports changes *after* that
point. Run a plain `list` first if you also want whatever was already
recorded.

**The reviewer can see this.** While `remax watching` runs they see a
mark in their header, and it changes to say whether you are blocked on
them or busy on their behalf. It goes out on its own within a few seconds
of that task ending, however it ended — so a watch you have quietly
dropped is not a secret, and one you forgot to stop is a lie you are
still telling.

## Both sides need the same store

The wait watches the verdict store, not the repository, and the store
lives under `XDG_STATE_HOME`. Ordinarily you and the reviewer both take
the default and it simply works; if you are sandboxing either side, they
have to point at the same one or you will wait forever on a store nobody
is writing to.
