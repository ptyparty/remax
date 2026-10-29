---
name: remax-review
description: "Act on code-review feedback recorded by remax — read the reviewer's flagged/questioned lines and notes for a repository, fix them, and understand how the review session sees your edits. Use when asked to address review feedback, check for remax flags, or work alongside a human reviewing in remax. Takes an argument: no argument answers what is outstanding once and stops; `watch` keeps waiting on the reviewer and reacting until told to stop; `stop` ends a watch."
---

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
*Staying and reacting* below — with one in flight the two of you work at
the same time rather than in turns.

## What you were asked for

The argument you were invoked with decides how long you stay:

**No argument — answer once.** Read what is outstanding, fix it, reply,
report what you did, and stop. Right when the human has finished a pass
and wants it dealt with. Do not start a background wait.

**`watch` — stay and react.** Start by saying you are here:

```sh
remax -C "$REPO" watching     # background task; runs until stopped
```

That holds a mark in the reviewer's header for as long as it runs, so
they can see there is somebody on the other end. Then do the one-off pass
— nothing already recorded should be left sitting — and from there keep a
`list --wait` in flight, answering each thing as they flag it, until they
stop you. Right when they are reviewing *now* and you are working
alongside them.

The two are separate on purpose. `watching` says an agent is attending;
the wait says what it is doing. With both running the reviewer sees
*working* while you read and edit and reply, and *watching* while you are
blocked on them — and nothing at all a few seconds after you die.

**`stop` — end the watch.** Kill the outstanding wait *and* the
`watching` task, and say so. Leaving the latter running tells them
somebody is on this when nobody is. Also do this without being asked if
the human says they are done reviewing, or closes the session.

## Reading the feedback

```sh
remax -C /path/to/repo list --awaiting agent --json
```

This prints what is waiting on you (run it from anywhere — `-C` points
at the target). Drop `--awaiting agent` to see every conversation,
including ones you have answered and ones with no note yet. **Two kinds
of thing can be waiting**: `tour_requests`, where the reviewer asked
you for a guided tour of the view they have open (see *Giving a tour*),
and `conversations`, each the lines a note was written across and the
thread about them. An empty `conversations` with a non-empty
`tour_requests` is work. Shape:

```json
{"repo": "/path/to/repo",
 "tour_requests": [
   {"repo": "/path/to/repo", "view": "feature vs b9045d7",
    "ref": "feature", "base": "b9045d7", "prompt": "schema changes first",
    "asked_at": "2026-10-22 19:48:58"}],
 "commits": {"8b36d626c": "Lists the air carriers an integration supports"},
 "conversations": [
   {"id": "3f9c…",
    "repo": "/path/to/repo",
    "file": "src/Foo.hs",
    "commit_sha": "8b36d626c",
    "lines": [{"line_no": 42, "line": "+  let xs = map fromJust ys", "verdict": "flagged"},
              {"line_no": 43, "line": "+  in sum xs", "verdict": "questioned"}],
    "thread": [{"author": "reviewer", "body": "use mapMaybe here instead"},
               {"author": "agent", "body": "done — switched to mapMaybe"}],
    "awaiting": "reviewer",
    "lines_in_file": "none",
    "updated_at": "2026-08-23 19:23:51",
    "pull": null}]}
```

### Each line's marker says which side it is

Each entry in `lines` has a `line` with a diff marker on the front, and
the marker tells you what kind of feedback this is:

- `+` — a line your change added. Ordinary feedback on your work.
- `-` — a line your change removed.
- **a space** — a line nobody has changed. This is a *remark on existing
  code*: the reviewer read past your diff, noticed something in the
  surrounding file, and asked about it there.

A remark is not about your change, so do not assume the fix belongs in
it. It may be a question about why something is the way it is, worth
answering rather than editing. If you do change the line, it becomes part
of your diff and comes back to them as a hunk to review — the remark and
its conversation follow it there, because a remark is keyed against the
committed file rather than the working one.

### When `-C` points at a folder of checkouts

A reviewer may run one remax over a directory that *holds* repositories
rather than being one — a story directory with a checkout per repo the
story touches — and switch between them with `:repo`. Point `-C` at
whatever directory the human named and it works either way: at a
repository you get that repository, at a folder of them you get **all of
them at once**, which is usually what you want, since one story's
feedback is spread across its repos.

That is why every conversation has its own **`repo`**. Read it rather
than the top-level one: when the listing spans checkouts, the top level
names the directory you asked about, and only `conversation.repo` tells
you which checkout a conversation lives in. `cd` there before editing the
file named by `conversation.file`, which is always relative to
`conversation.repo`.

`remax reply ID TEXT` works from the folder too — an id belongs to
exactly one repository, so remax finds the owner itself and tells you
which one it landed in.

- `id` — the conversation's address for `remax reply` and
  `remax comment --revise`: a GitHub thread's own id (`PRRT_…`) or a
  note's, which a note keeps when the reviewer grows it over more lines.
  It is not a line key; hold only a line, and `list --json` is how you
  find its conversation.
- `kind` — `"thread"` for a GitHub review thread the session has polled,
  `"note"` for a `!`/`?` note between you and the reviewer, `"draft"`
  for a comment pending until the reviewer submits, `"pull"` for the
  PR-level comments and `"review"` for a review summary.
- `lines` — the lines the conversation is about, by `line_no`. The `-`
  and `+` sides of a change are numbered separately, so they can
  interleave; pair them by content, not by position. Each has its own
  `verdict`, and the lines of one conversation can differ:
  - `"flagged"` (`!`) — the reviewer wants this changed. Fix it.
  - `"questioned"` (`?`) — the reviewer wants an explanation or a fix.
    If the code is wrong, fix it. If the code is right as written,
    answer the question — see **Replying to notes** below. Never answer
    by adding comments to the code, and never edit the reviewer's note.
- `line` — the exact line text, prefixed `+` (added), `-` (removed from
  the base) or a space (unchanged). Find it by content.
- `line_no` — where the line sat when the verdict was recorded;
  advisory, may have drifted. Trust `line` over `line_no`.
- `thread` — the conversation, oldest first. Each post has an `author`
  (`"reviewer"` is the human; a GitHub login is another reviewer; agents
  appear under the name they replied with), a `body` and a `state`:
  `"posted"` on GitHub, `"pending"` until the reviewer submits,
  `"proposal"` for text you proposed, `"local"` for talk between you and
  the reviewer that never reaches GitHub. **Read the whole thread and act
  on the newest reviewer post**; if your own post is already the last
  word and still answers it, don't repeat yourself.
- `awaiting` — whose turn it is. `"agent"`: the reviewer or a GitHub
  reviewer spoke last, so act on it. `"reviewer"`: your post is the last
  word, so leave it unless they answer. `"note"`: a verdict with no note
  yet — see *A conversation with no note yet*.
- `pull` — where the conversation lives: the pull request number for a
  GitHub thread, PR-level comment, review or draft. `null` on a note means
  repo-wide: it shows in every view that has its lines. `null` on a draft
  means it was written with no pull request in view; see *Drafting review
  comments*. Every
  post on a conversation, yours and the reviewer's, lands where the
  conversation lives, whatever view either of you is in.
- `lines_in_file` — whether the conversation's `+` and unchanged lines
  still stand together in the working file, spaced as their `line_no`s
  were: `"all"`, `"some"` or `"none"`. The block may have moved up or
  down. `"none"` usually means the code was already changed or deleted,
  by you or in another commit. Check that the thread's point was met,
  then say so with `reply` rather than editing again. `null` when there
  is nothing distinctive to look for: every line is a `-` line, blank,
  or only punctuation such as `}`.
- `commit_sha` — the commit the note was written on, when the session
  knew one. `null` otherwise; recover the introducing commit yourself
  from the line text when it matters (`git log -S"<line text>"` or
  `git blame`).

One `id` can head more than one conversation. That happens when some of
a note's lines differ from the rest in thread, file or commit: the
reviewer edited or added a note over only some of them, or re-verdicted
some at a later commit. Read each thread. It is still one piece of
feedback: fix it once and reply once. A reply to the `id` reaches every
line under it, in all of them.

## Staying and reacting (`watch`)

`list` answers with whatever is recorded right now. `--wait` prints
nothing until the punch list *changes*, then prints it and exits. Run it
as a **background task** and your harness wakes you when the reviewer
does something — you react to a flag the way they react to your edits,
and neither of you has to prompt the other.

Start one as soon as the one-off pass is done, and **always pass
`--state`**:

```sh
SEEN="/tmp/remax-seen$(printf %s "$REPO" | tr / -)"
FEEDBACK="/tmp/remax-feedback$(printf %s "$REPO" | tr / -).json"
remax -C "$REPO" list --wait --state "$SEEN" --json > "$FEEDBACK.part" &&
  mv "$FEEDBACK.part" "$FEEDBACK"
```

The rename keeps `$FEEDBACK` holding the last complete listing while
the next wait runs. Reading it mid-wait gives you what you last saw
rather than an empty file.

Only the reviewer wakes a wait: a verdict, a note, or a `:tour ask`.
Your replies and edits are not a change to it, so you never need to
wait twice to get past your own work. When it wakes, act on
**everything** in the listing, `tour_requests` included — a request
you skip is already recorded as seen, and no later wait will show it
again.

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

### A conversation with no note yet

Recording a verdict and writing its note are two separate events: remax
records the verdict the instant the key is pressed, so the reviewer's
tray and meters answer at once, while they are still typing the sentence
saying what is wrong. A wait holds while a note is being written, so
ordinarily you are woken once, with it:

```json
{"lines": [{"line": "+TWO CHANGED", "verdict": "flagged"}],
 "thread": [{"author": "reviewer", "body": "use a constant"}]}
```

You can still meet a conversation whose `thread` is `[]` — a bare `!`
with the note popup turned off, or one they opened and abandoned. There
is nothing to act on. Do not guess at what they meant: say you are
holding off, and wait again with the same `--state` file.

Never answer a flag whose note you have not read. Your reply would land
*ahead* of theirs in the thread, which reads as a non-sequitur to them.

### One flagged line, two sides

Flagging a line that was *changed* covers both sides of the change, so
its conversation has two lines — the removal and the addition:

```json
{"lines": [{"line": "-two", "verdict": "flagged"},
           {"line": "+TWO CHANGED", "verdict": "flagged"}],
 "thread": [{"author": "reviewer", "body": "use a named constant"}]}
```

That is one piece of feedback. Fix it once, and `reply` once to its
`id`.

### Both sides need the same store

The wait watches the verdict store, not the repository, and the store
lives under `XDG_STATE_HOME`. Ordinarily you and the reviewer both take
the default and it simply works; if you are sandboxing either side, they
have to point at the same one or you will wait forever on a store nobody
is writing to.

## Replying to notes

```sh
remax -C /path/to/repo reply ID "answer text"
```

`ID` is the conversation's `id` from `list --json`, a GitHub thread's
or a note's. The reply **appends a local post to that conversation** and
renders inline, in order and under your name, in the reviewer's live
session — this is the primary way to answer a `?`, because the answer
appears exactly where the question was asked. A reply never reaches
GitHub: on a GitHub thread it is talk beside the thread that only the
reviewer sees. Also summarize the answer in your chat reply when
you're in a conversation with the human. The thread is ordered: the
reviewer may answer your post with another of theirs, so re-check
`list --json` for posts newer than yours instead of assuming your
answer was the last word. Accepting the hunk (or resolving the note)
closes the whole conversation. The output says where the post landed,
`repo-wide` for a note or `on PR #N` for anything bound for GitHub. A
reply to a verdict with no note yet starts a repo-wide note.

## Drafting review comments

```sh
remax -C /path/to/repo comment HUNK_ID "comment text"   # draft on a hunk
remax -C /path/to/repo comment --revise ID "new text"   # propose a rewrite
```

`HUNK_ID` comes from `remax hunks`. `comment` writes a GitHub comment on
the hunk's lines in the reviewer's name, marked as written by you,
pending until they submit it; they can edit or drop it first. `--revise`
records a proposal on a conversation by its `id`: text for a reply, or
for a draft of theirs. Nothing happens until the reviewer takes it with
Enter, which makes it their own pending text, and a new proposal
replaces the old. Draft or revise only when asked to.

A draft belongs to the pull request it was written for, and the output
names it with the draft's id: `comment 3f9c… proposed on src/Foo.hs:42 on
PR #482`. A draft written with no pull request in view belongs to none
and shows in every pull request until it is submitted or moved. When a
pull request is closed and reopened under a new number, its drafts stay
with the old one; move each draft, with every post on it, when the
reviewer asks:

```sh
remax -C /path/to/repo move DRAFT_ID --pr 512
```

Only drafts move. A GitHub thread, PR-level comment or review lives on
the pull request GitHub keeps it on, and a note is repo-wide.

The session writes each poll of the pull request to a cache file that
`list` reads, so GitHub threads appear in the listing while a session
has the pull request open, and `list --wait` wakes on a new GitHub post
as on a new note.

## Addressing feedback on the reviewer's own pull request

`:pr edit` checks the reviewer's pull request out, and the session's
`view` reads `PR #N edit`. The files are the working tree, so fix them as
for any flag.

## Giving a tour

When the human asks you to walk them through a change — present it,
explain it, "give me a tour" — build one and remax walks them along it.
This is the one thing you post *before* being asked about a line.

They can also ask from inside remax. A `tour_requests` entry in
`list --json` (or a `tour requested for …` line in the text listing) is
that ask, and it wakes a `list --wait` like a flag does:

```json
{"tour_requests": [
   {"repo": "/path/to/repo", "view": "feature vs b9045d7",
    "ref": "feature", "base": "b9045d7", "prompt": "schema changes first",
    "asked_at": "2026-10-22 03:10:41"}]}
```

Answer it with the tour below, passing `--ref` and `--base` exactly as
the request gives them (both `null` means the working tree: pass no
flags). `prompt` is their brief; follow it. Posting `tour begin` for
that view clears the request. Spelling the commit differently — the
branch name where the reviewer has a sha open, or a sha where they have
the branch — still reaches them, since remax matches tours by the
commit they resolve to.

```sh
remax -C "$REPO" hunks --json
```

Lists the change set exactly as the reviewer's session sees it: one
entry per hunk with an `id`, `file`, `line_no` and the marked `lines`.
With no flags it follows **whatever their open session is viewing** —
the working tree, a `:ref`, a `:commit` — so you do not need to be told
which commit they have up; `--ref REF [--base BASE]`, `--base REF` or
`--pr N` names a view when nobody has one open. A pull request opened
with `--pr N` or `:pr N` is a subject of its own: the listing's `subject`
reads `pull/N`, and the tour stays with the pull request as its head and
base move. Read the whole listing and decide
an order and a sentence or two for each stop; a stop may cover several
hunks when they are one idea (a rename and its fallout, a signature and
its call sites).

```sh
remax -C "$REPO" tour begin
remax -C "$REPO" tour step --title "the new table" ID "Start here; everything else hangs off it"
remax -C "$REPO" tour step --title "call sites" ID,ID,ID "The three call sites the signature change touched"
```

`--title` is a few words naming the stop. Every hunk in the stop gets a
header reading `stop 3/12 · call sites`, so a reviewer who lands on
one of its hunks out of order still knows where they are; the note
itself shows only on the first hunk. Give every stop a title when the
tour has more than a handful.

`tour begin` empties any earlier tour for that view. Each `tour step`
appends one stop; the reviewer's session switches to following the
tour as the first one lands and each note renders above its hunk on
its own background. An id that is not in the view is refused, so re-run
`hunks` after you edit.

Keep a stop small: one hunk, or the two or three that are one idea.
`]`/`[` walk the hunks inside a stop one by one and the header counts
both the stop and the position within it, so a twenty-hunk stop reads
as twenty hunks with one note at the front. Split by idea instead.

Keep notes short and pointed at the code — what to look at and why it
is there, not a restatement of the diff. The reviewer still reads every
line; the tour is the order and the context, not the review. Do not
turn feedback into tour steps: a `!` or `?` is theirs to give.

## Fixing per-commit feedback

When conversations have a `commit_sha`, the reviewer reviewed commit by
commit and the fix should usually land in that commit, not at the branch
tip. Group conversations by `commit_sha` and either amend surgically
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
- Post a tour only when asked for one; `tour begin` discards the
  previous tour for that view.
- An empty `conversations` list with an empty `tour_requests` list
  means no outstanding feedback. A tour request on its own is work.
- Under `watch`, keep one `list --wait` in flight and start a fresh one
  each time it wakes you. With none running you are only as current as
  the last time you asked. Without `watch`, do not start one at all —
  a background task the human did not ask for outlives their attention.
