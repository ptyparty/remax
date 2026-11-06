# When the review moves

A `moved` line says the reviewer's session has moved from the checkout
you are watching into another one, or back: the path after
`review is in` is the checkout the review is in now, called `to` below,
and `PR #N` the pull request whose worktree it entered or left. The verdicts, notes
and drafts the reviewer makes there are keyed by that path, so a `list`
on the old path never shows them, and an edit made at the old path
lands on the wrong branch. A checkout and the pull request worktrees
made from it are one group, and every path in the group reports the
group's latest move, until the next replaces it. The entry is a fact
about where the review is, not a request: it stays in every listing
until the next move, so a move you have already followed is not
outstanding work. A later entry with a newer time is.

What to do depends on whether your `-C` already covers `to`. It covers
`to` when `-C` is `to` itself, or when `-C` is a folder and `to` is a
directory directly under it:

- **`-C` is a folder with `to` directly under it** (the worktree sits beside the story's checkouts, at the default `../<repo>-pr-<N>`): keep `-C` and the watch. The listing spans the worktree too, and the move is reported once per group. Find its work under the worktree's `── name ──` heading, and `cd` there to edit. If you started the watch before the worktree existed, restart it. A watch reads the folder's checkouts when it starts, so until it is started again the reviewer's header in the worktree shows nobody watching, and a flag filed there prints nothing.
- **Otherwise**: re-point `-C` to `to`. Stop the watch, start it again on `to`, and edit files there. Do the same on the way back when a later move names the original checkout. The first block of the new watch is everything awaiting you at `to`, the move you just followed included, because a move stays listed. Without a Monitor tool, restart `watching` on `to` as well, and name new state and feedback files after `to`, because the fingerprint is per path.

A move that enters a worktree names the pull request; the worktree is
already on its branch. Nothing is pushed or posted when the review
moves. Quitting remax writes no move: the review stays where it was
until a session moves it.
