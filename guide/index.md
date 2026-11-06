# Working with a remax reviewer

remax is a hunk-by-hunk review TUI the human keeps open while you work.
It watches the filesystem: **every edit you make appears in their session
live**, and lines you touch automatically return to "pending" for them —
verdicts are keyed by line content, so changing a line resets it. You
never need to notify the reviewer or run any command after fixing
something.

The other direction is not automatic unless you make it so. Nothing tells
you a line was flagged; either the human asks you to look, or you keep a
`list --wait` running in the background and are woken when they act. See
[Staying and reacting](watch.md) — with one in flight the two of you
work at the same time rather than in turns.

This guide is split into pages. Each link below names one: `remax guide
watch` prints `watch.md`. Read the pages the task needs, not all of them.

## Answering once, or staying

**Answer once.** Read what is outstanding, fix it (unless the view is a
review, see [Answering in a pull request
review](pull-requests.md#answering-in-a-pull-request-review)), reply,
report what you did, and stop. Right when the human has finished a pass
and wants it dealt with. Do not start a background wait.

**Stay and react.** Start a watch on the review, as [Staying and reacting](watch.md) shows:

```sh
remax -C "$REPO" watch --timeout 1790 2>&1     # with the Monitor tool
```

The watch holds a mark in the reviewer's header for as long as it runs, so they can see there is somebody on the other end. Its first block is everything already recorded, so nothing is left sitting, and from there it prints a block each time the reviewer acts, until they stop you. Right when they are reviewing *now* and you are working alongside them. The mark says *working* while anything awaits you and *watching* once you have answered it all, and it goes out a few seconds after the watch ends.

**End a watch.** Stop the watch, and say so. Leaving it running tells the reviewer somebody is on this when nobody is. Also do this without being asked if the human says they are done reviewing, or closes the session. Without a Monitor tool the watch is two background tasks, `watching` and a `list --wait`, and ending it means killing both.

## Pages

- [Reading the feedback](feedback.md): `remax list` and `show`, what
  each part of a conversation means, and folders of checkouts. Read it
  before acting on any feedback.
- [Staying and reacting](watch.md): `remax watch`, for keeping up with one reviewer or several while they work.
- [Replying and opening notes](reply.md): `remax reply` and
  `remax note`.
- [Drafting review comments](comments.md): `remax comment` and
  `remax move`, for GitHub comments the reviewer submits.
- [Pull requests](pull-requests.md): answering in a read-only review,
  fixing the reviewer's own pull request, and reading one with local git.
- [When the review moves](moves.md): following a `moved` line into
  another checkout.
- [Giving a tour](tour.md): walking the reviewer through a change, and
  answering a `tour` line.
- [Pointing the reviewer](pointing.md): `remax do :go` and
  `:highlight`, including whenever the human asks to be shown where some
  code is.
- [Fixing per-commit feedback](per-commit.md): landing a fix in the
  commit it was flagged on.

## Rules

- Anything you have to say back to the reviewer goes through
  `remax reply` (and the chat when one is open): an answer to a `?`,
  pushback on a `!` you think is wrong, a heads-up that you fixed it a
  different way than they suggested. The reviewer opened a conversation,
  so reply in the conversation, where it renders next to the line they
  asked about — never as comments in source files unless the reviewer
  explicitly asks for a comment, and never by editing the reviewer's
  note text.
- Fix by editing the files normally. The reviewer's session updates
  itself; the fixed lines go back to pending and they re-review.
- In a `PR #N review` view, never edit files or offer fixes: answer
  with `remax reply`, and draft comments only when asked.
- Never write to `~/.local/state/remax/review.db` directly, and never
  run `remax forget` unless the human explicitly asks.
- Post a tour only when asked for one; `tour begin` discards the
  previous tour for that view.
- No conversations and no `tour` line means no outstanding feedback,
  whatever `moved` line the listing has: a move
  you have followed stays listed. A tour request on its own is work,
  and so is a move you have not followed yet: follow it as [When the
  review moves](moves.md) says.
- While staying, keep exactly one watch in flight on each review. With none running you are only as current as the last time you asked. When answering once, do not start one at all: a background task the human did not ask for outlives their attention.
