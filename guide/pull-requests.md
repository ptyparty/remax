# Pull requests

## Answering in a pull request review

When the session's `view` reads `PR #N review`, the reviewer is
reading a pull request without changing it, usually someone else's, so
nothing in it is yours to fix. `remax -C "$REPO" hunks` names the view
on its first line, as `viewing PR #482 review`, and `hunks --json` in
its `view` field. In that view:

- Answer every note with `remax reply`. Explain the code, say whether
  the reviewer's concern holds, or say what the author could change. A
  `!` there asks you to judge the line, not to change it.
- Draft a GitHub comment with `remax comment` only when the reviewer
  asks for one.
- Never edit files, commit, or offer to fix anything. A fix belongs on
  the author's branch, and when the pull request is the reviewer's own,
  `:pr edit` is theirs to run.

## Addressing feedback on the reviewer's own pull request

`:pr edit` opens the reviewer's pull request for editing, and the
session's `view` reads `PR #N edit`. The files are the working tree, so
fix them as for any flag. Unless the checkout was already on the pull
request's branch, the session moves into a git worktree of its own for
that pull request, and tells you so; see the next section.

## Reading a pull request

When your remax work needs a pull request's code, such as for a tour
or to answer a note, read it with local git rather than `gh`. A pull
request remax has opened is already fetched: its head is
`refs/remax/pull/N/head` and its base `refs/remax/pull/N/base`, the
merge-base, so the diff between them is the one GitHub shows. Read it as you would any branch:

```sh
git -C "$REPO" log --reverse refs/remax/pull/N/base..refs/remax/pull/N/head
git -C "$REPO" diff refs/remax/pull/N/base refs/remax/pull/N/head -- src/Foo.hs
```

`git show` and `git blame` work on those refs too. They are as fresh as
the last fetch: an open session on the pull request keeps them current,
and with none, `remax -C "$REPO" hunks --pr N` fetches the head again.
Run that first rather than fetching or checking out the pull request
yourself.

When the session's `view` reads `PR #N edit`, the pull request is
checked out, usually in a worktree of its own (see [When the review
moves](moves.md)), and that checkout's working tree is its head, holding commits
and edits not pushed yet. Read that checkout, diffed against
`refs/remax/pull/N/base`, rather than the head ref.

While a session has the pull request open, its review threads and
PR-level comments are in `remax list`, and `show` says `on PR #N` for
each of them.

For that work, use `gh` only for what git does not hold, such as the
description or a failing check's log, and never for the diff. Outside
remax work, use `gh` as you normally would.
