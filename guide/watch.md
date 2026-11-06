# Staying and reacting

`list` answers with whatever is recorded right now. To keep up with a reviewer while they work, run `remax watch`, which keeps printing: what awaits you when it starts, then a block each time that changes.

## With a Monitor tool

In Claude Code, run it with the **Monitor** tool, once, with `timeout_ms` 1800000:

```sh
remax -C "$REPO" watch --timeout 1790 2>&1
```

Give the Monitor a description naming the review, such as `remax review of myrepo`. Every block arrives as its own event while you keep working, so you never start another wait after acting on one. When it prints `timed out` and exits, start it again. Never pipe it or redirect its stdout: each block is recorded as seen when it is printed, so a block sent anywhere else is one you never read.

Each block starts with a line naming the review and what is in it:

```
remax -C /home/me/myrepo: 2 conversations, 1 new, 1 changed
```

and then the listing, as `list --awaiting agent` prints it, with each conversation marked `new` or `changed`. The `-C` path in that line is the one to pass to `reply`, `show`, `note` and `do` for everything in the block. The first block is everything that awaits you, so it is also the one-off pass, and `nothing awaits you` when there is none. Act on **everything** in each block, `tour` and `moved` lines included: a block is not shown again once printed.

Only the reviewer wakes it: a note, a reply in a thread, a GitHub post, a `:tour ask`, or a move of the review into another checkout. Your reply hands its conversation to the reviewer, which drops it from what awaits you, so your own work never prints a block.

**The reviewer can see this.** For as long as the watch runs they see a mark in their header, and it goes out a few seconds after the watch ends, however it ended. The mark says *working* while anything awaits you and *watching* once nothing does, so it moves as you reply. A conversation you fixed but did not reply to keeps it at *working*: the reviewer is still waiting on you. The watch also exits when whatever started it goes away.

remax remembers what the last watch on this review printed, in a file named after the review under its state directory, and marks `new` and `changed` against it after a restart. `--state FILE` keeps it somewhere else.

## Without one

Where the harness has no Monitor, such as pi, use two background commands instead. `watching` holds the mark:

```sh
remax -C "$REPO" watching     # background task; runs until stopped
```

and `list --wait` prints nothing until what awaits you *changes*, then prints it and exits. Run it as a background task, so your harness wakes you when it exits, and **always pass `--awaiting agent` and `--state`**:

```sh
SEEN="/tmp/remax-seen$(printf %s "$REPO" | tr / -)"
FEEDBACK="/tmp/remax-feedback$(printf %s "$REPO" | tr / -)"
remax -C "$REPO" list --wait --awaiting agent --state "$SEEN" \
  > "$FEEDBACK.part" && mv "$FEEDBACK.part" "$FEEDBACK"
```

The rename keeps `$FEEDBACK` holding the last complete listing while the next wait runs. A wait is a single shot: read the file, act on everything in it, and **start another one**. While none is running you are only as current as the last time you asked. It blocks from the moment it starts, so run a plain `list` first for whatever was already recorded.

Without `--awaiting agent` every change counts, and each of your replies wakes the wait at once. The state file is where remax keeps a fingerprint of the listing you were last shown. Without it a wait measures change from the moment it starts, so whatever the reviewer did while you were reading and writing is absorbed into the new baseline and you sit through it. **Name both files after the review**, as above: the fingerprint has to survive from one of your waits to the next, so a fixed path is shared with every other agent on the machine watching some other review, and two agents writing one output path overwrite each other's feedback.

The two halves show the reviewer different things: *working* from `watching` while you read and edit and reply, and *watching* while the wait is blocked on them.

## Several reviews at once

Run one `remax watch` per review, each in its own Monitor with a description naming it. Blocks from all of them arrive in one conversation, interleaved, so:

- Take the review from the block's first line every time. Conversation ids are short only within their own review, and the same prefix can mean another conversation in the next block.
- Finish the conversation in hand, reply included, before turning to a block from another review. A reply sent with the wrong `-C` lands in the wrong review, or nowhere.
- A folder of checkouts is one review already: one watch from the folder covers every checkout under it, with each conversation listed under its checkout's name. Do not also watch the checkouts one by one, or every block arrives twice.
- Two sessions on one checkout, such as the working tree and `--branch`, share what is recorded there, and one watch covers both.
- Keep exactly one watch on any review, across agents too. Two watches on one review both print every block, and the feedback gets answered twice.

Each review's header says *working* while its feedback is with you, even while you are busy on another review. It tells the reviewer whose move it is, not what you are doing this minute. When one review will sit waiting for a while because another is taking all your attention, say so in a reply to it.

Each Monitor expires on its own and is re-armed on its own. Stop the watches on the reviews that end and leave the others running.

## Both sides need the same store

The watch reads the verdict store, not the repository, and the store lives under `XDG_STATE_HOME`. Ordinarily you and the reviewer both take the default and it simply works; if you are sandboxing either side, they have to point at the same one or you will wait forever on a store nobody is writing to.
