---
name: remax-review
description: Act on code-review feedback recorded by remax — read the reviewer's flagged/questioned lines and notes for a repository, fix them, and understand how the review session sees your edits. Use when asked to address review feedback, check for remax flags, or work alongside a human reviewing in remax.
---

# Working with a remax reviewer

remax is a hunk-by-hunk review TUI the human keeps open while you work.
It watches the filesystem: **every edit you make appears in their session
live**, and lines you touch automatically return to "pending" for them —
verdicts are keyed by line content, so changing a line resets it. You
never need to notify the reviewer or run any command after fixing
something.

## Reading the feedback

```sh
remax -C /path/to/repo list --json
```

This prints the punch list for that repository (run it from anywhere —
`-C` points at the repo). Shape:

```json
{"repo": "/path/to/repo",
 "items": [
   {"file": "src/Foo.hs",
    "verdict": "flagged",
    "line": "+  let xs = map fromJust ys",
    "line_no": 42,
    "note": "use mapMaybe here instead",
    "thread": [{"author": "reviewer", "body": "use mapMaybe here instead"},
               {"author": "agent", "body": "done — switched to mapMaybe"}],
    "updated_at": "2026-08-23 19:23:51"}]}
```

- `verdict: "flagged"` (`!`) — the reviewer wants this changed. Fix it.
- `verdict: "questioned"` (`?`) — the reviewer wants an explanation or a
  fix. If the code is wrong, fix it. If the code is right as written,
  answer the question — see **Replying to notes** below. Never answer
  by adding comments to the code, and never edit the reviewer's note.
- `thread` — the conversation on this item, oldest first. Each post
  has an `author` (`"reviewer"` is the human; agents appear under the
  name they replied with) and a `body`. **Read the whole thread and act
  on the newest reviewer post**; if your own post is already the last
  word and still answers it, don't repeat yourself.
- `note` / `reply` — compatibility views of the thread: the opening
  reviewer post and the newest non-reviewer post. Prefer `thread`.
- `key` — the item's stable identifier; the address for `remax reply`.
- `line` — the exact line text, prefixed `+` (added) or `-` (removed
  from the base). Find it by content.
- `line_no` — where the line sat when the verdict was recorded;
  advisory, may have drifted. Trust `line` over `line_no`.
- `commit_sha` / `commit_subject` — set when the feedback was given
  during a commit-by-commit rebase review: that is the commit the
  feedback is about. `null` for ambient reviews; recover the introducing
  commit yourself from the line text when it matters
  (`git log -S"<line text>"` or `git blame`).

## Replying to notes

```sh
remax -C /path/to/repo reply KEY "answer text"
```

`KEY` is the item's `key` from `list --json`. The reply **appends a
post to the item's conversation** and renders inline, in order and
under your name, in the reviewer's live session — this is the primary
way to answer a `?`, because the answer appears exactly where the
question was asked. Also summarize the answer in your chat reply when
you're in a conversation with the human. The thread is ordered: the
reviewer may answer your post with another of theirs, so re-check
`list --json` for posts newer than yours instead of assuming your
answer was the last word. Accepting the hunk (or resolving the note)
closes the whole conversation.

## Fixing per-commit feedback

When items carry `commit_sha`, the reviewer reviewed commit by commit
and the fix should usually land in that commit, not at the branch tip.
Group items by `commit_sha` and either amend surgically
(`git rebase --exec false <base>`, fix at each relevant stop,
`git commit -a --amend --no-edit`, `git rebase --continue`) or make
fixup commits (`git commit --fixup <sha>` per group, then
`git rebase --autosquash <base>`). If history rewriting is risky in
context (shared branch), ask the human before rewriting; fixing at the
tip is the safe fallback.

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
- Never write to `~/.local/state/remax/review.db` directly, and never
  run `remax forget` unless the human explicitly asks.
- An empty `items` list means no outstanding feedback.
