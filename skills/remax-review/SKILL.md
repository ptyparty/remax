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

This prints the punch list (run it from anywhere — `-C` points at the
target). Shape:

```json
{"repo": "/path/to/repo",
 "items": [
   {"file": "src/Foo.hs",
    "repo": "/path/to/repo",
    "verdict": "flagged",
    "line": "+  let xs = map fromJust ys",
    "line_no": 42,
    "note": "use mapMaybe here instead",
    "thread": [{"author": "reviewer", "body": "use mapMaybe here instead"},
               {"author": "agent", "body": "done — switched to mapMaybe"}],
    "updated_at": "2026-08-23 19:23:51"}]}
```

### When `-C` points at a folder of checkouts

A reviewer may run one remax over a directory that *holds* repositories
rather than being one — a story directory with a checkout per repo the
story touches — and switch between them with `:repo`. Point `-C` at
whatever directory the human named and it works either way: at a
repository you get that repository, at a folder of them you get **all of
them at once**, which is usually what you want, since one story's
feedback is spread across its repos.

That is why every item carries its own **`repo`**. Read it rather than
the top-level one: when the listing spans checkouts, the top level names
the directory you asked about, and only `item.repo` tells you which
checkout an item lives in. `cd` there before editing the file named by
`item.file`, which is always relative to `item.repo`.

`remax reply KEY TEXT` works from the folder too — a key belongs to
exactly one repository, so remax finds the owner itself and tells you
which one it landed in.

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

## Waiting for feedback instead of asking for it

`list` answers with whatever is recorded right now. `--wait` prints
nothing until the punch list *changes* and then prints it and exits, so a
long-running agent can sit on the reviewer rather than poll them:

```sh
while remax -C "$REPO" list --wait --json > /tmp/feedback; do
  # ... read /tmp/feedback and act ...
done
```

It blocks from the moment it starts, so it reports changes *after* that
point — take an ordinary `list` first if you also want what is already
there.

**Wait for the note, not just the flag.** Flagging a line and writing its
note are two separate events: remax records the verdict the instant the
key is pressed, and the reviewer is still typing. Waking on the first
change gets you a flagged line with `"note": null` — you would be
answering before being told what is wrong, and your reply lands *ahead*
of their note in the thread. Keep waiting until the item you care about
has a note:

```sh
while :; do
  remax -C "$REPO" list --wait --json > /tmp/feedback
  # ... stop when the item you are waiting on has a non-null note ...
done
```

`scripts/lib/hero_agent.py` in the remax repository is this loop written
out, and is what records the project's own demonstration.

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
