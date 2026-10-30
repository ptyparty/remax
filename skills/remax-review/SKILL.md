---
name: remax-review
description: "Act on code-review feedback recorded by remax — read the reviewer's flagged/questioned lines and notes for a repository, fix them (or only answer them in a read-only pull request review), and understand how the review session sees your edits. Use when asked to address review feedback, check for remax flags, or work alongside a human reviewing in remax, or to show the reviewer a hunk, file, line or conversation by moving their remax session there (`remax do :go`). Also use whenever the user asks to be shown where some code is: take them there instead of replying with a line number. Takes an argument: no argument answers what is outstanding once and stops; `watch` keeps waiting on the reviewer and reacting until told to stop; `stop` ends a watch."
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

**No argument — answer once.** Read what is outstanding, fix it (unless
the view is a review, see *Answering in a pull request review*), reply,
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
remax -C /path/to/repo list --awaiting agent
```

This prints what is waiting on you (run it from anywhere — `-C` points
at the target). Drop `--awaiting agent` to see every conversation,
including ones you have answered and ones with no note yet. **Three
kinds of thing can be waiting**: a `moved` line, where the review has
moved into another checkout (see *When the review moves*), a `tour`
line, where the reviewer asked you for a guided tour of the view they
have open (see *Giving a tour*), and conversations, each the lines a
note was written across and the thread about them. A `tour` line or a
`moved` line with no conversations under it is still work. Shape:

```
moved  PR #482 review is in /path/to/repo-pr-482  (2026-10-29 18:02:11)
tour   feature vs b9045d7: schema changes first  (--ref feature --base b9045d7)

3f9c41d2  note  src/Foo.hs  @8b36d626  awaiting agent  new
  ! 42 +  let xs = map fromJust ys
  ? 43 +  in sum xs
  (1 earlier posts)
  reviewer: use mapMaybe here instead
  claude: done — switched to mapMaybe
```

Each conversation is a header line, its lines, and its thread:

- **Header:** the id, the kind, the file (`PR #N` for a conversation
  on the pull request as a whole), the commit after `@`, whose turn it
  is, `in file: some` or `in file: none` when the lines have changed,
  and on a wait's wake `new` or `changed`.
- **Lines:** a verdict glyph, the line number and the line, at most six
  of them, each cut at 160 characters.
- **Thread:** the last two posts, with earlier ones counted. A bot's
  post shows as its first line of text.

`remax -C /path/to/repo show ID` prints one conversation whole: every
line and every post in full, with where it lives and when it last
changed. Run it whenever the listing cut something you need.

`list --json` prints every conversation whole, for scripts. Read the
plain listing yourself.

### Each line's marker says which side it is

Each line's text has a diff marker on the front, after its number, and
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

That is why a listing that spans checkouts heads each checkout's
conversations with its name, as `── penguin ──`. `cd` into that
checkout before editing a conversation's file, which is always relative
to it. `show` names the checkout in full.

`remax reply ID TEXT` works from the folder too — an id belongs to
exactly one repository, so remax finds the owner itself and tells you
which one it landed in.

- **id** — the conversation's address for `remax reply`, `remax show`,
  `remax comment --revise` and `remax move`: a GitHub thread's own id
  (`PRRT_…`) or a note's, which a note keeps when the reviewer grows it
  over more lines. The listing prints the shortest prefix no other
  conversation shares, at least eight characters, and every command
  takes any such prefix or the whole id. It is not a line key.
- **kind** — `thread` for a GitHub review thread the session has
  polled, `note` for a `!`/`?` note between you and the reviewer,
  `draft` for a comment pending until the reviewer submits, `pull` for
  the PR-level comments and `review` for a review summary.
- **lines** — the lines the conversation is about. The `-` and `+`
  sides of a change are numbered separately, so they can interleave;
  pair them by content, not by position. Each has its own verdict, and
  the lines of one conversation can differ:
  - `!` (flagged) — the reviewer wants this changed. Fix it, except in
    a `PR #N review` view (see *Answering in a pull request review*).
  - `?` (questioned) — the reviewer wants an explanation or a fix.
    If the code is wrong, fix it, again except in a review view. If the
    code is right as written, answer the question — see **Replying to
    notes** below. Never answer
    by adding comments to the code, and never edit the reviewer's note.
  - a blank — a line with no verdict of its own, such as a GitHub
    thread's.
- The line text is exact, prefixed `+` (added), `-` (removed from the
  base) or a space (unchanged). Find it by content. The number is where
  the line sat when the verdict was recorded; it may have drifted, so
  trust the text over the number.
- **thread** — the conversation, oldest first, as `author: body`.
  `reviewer` is the human; a GitHub login is another reviewer; agents
  appear under the name they replied with. `[pending]` marks a post
  waiting for the reviewer to submit it and `[proposal]` text you
  proposed. **Act on the newest reviewer post**, and `show` the whole
  thread when earlier posts were counted rather than shown; if your own
  post is already the last word and still answers it, don't repeat
  yourself.
- **awaiting** — whose turn it is. `agent`: the reviewer or a GitHub
  reviewer spoke last, so act on it. `reviewer`: your post is the last
  word, so leave it unless they answer. `note`: a verdict with no note
  yet — see *A conversation with no note yet*. `--awaiting agent`
  leaves out PR-level comments, review summaries, and threads a bot
  spoke last on: none of those asks you for anything.
- **where it lives** — `show` says `on PR #N` for a GitHub thread,
  PR-level comment, review or draft, and `repo-wide` for a note, which
  shows in every view that has its lines. A draft that is `repo-wide`
  was written with no pull request in view; see *Drafting review
  comments*. Every post on a conversation, yours and the reviewer's,
  lands where the conversation lives, whatever view either of you is
  in.
- **in file** — whether the conversation's `+` and unchanged lines
  still stand together in the working file, spaced as their numbers
  were. The header shows `in file: some` when only part of them do and
  `in file: none` when none do; the block may have moved up or down.
  `none` usually means the code was already changed or deleted, by you
  or in another commit. Check that the thread's point was met, then say
  so with `reply` rather than editing again. Nothing shows when they
  all stand, or when there is nothing distinctive to look for: every
  line is a `-` line, blank, or only punctuation such as `}`.
  `lines_in_file` in `list --json` gives `"all"`, `"some"`, `"none"`
  or `null`.
- **commit** — the commit the note was written on, when the session
  knew one. Absent otherwise; recover the introducing commit yourself
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

### A conversation with no note yet

Recording a verdict and writing its note are two separate events: remax
records the verdict the instant the key is pressed, so the reviewer's
tray and meters answer at once, while they are still typing the sentence
saying what is wrong. A verdict with no note waits on `note`, so
`--awaiting agent` leaves it out and it does not wake the wait. Its
note does, once written:

```
5a1e09c7  note  src/Foo.hs  awaiting agent  new
  ! 12 +TWO CHANGED
  reviewer: use a constant
```

A listing without `--awaiting agent` can still show a conversation
with no posts — a bare `!` with the note popup turned off, or
one they opened and abandoned. There is nothing to act on. Do not guess
at what they meant: say you are holding off, and wait again with the
same `--state` file.

Never answer a flag whose note you have not read. Your reply would land
*ahead* of theirs in the thread, which reads as a non-sequitur to them.

### One flagged line, two sides

Flagging a line that was *changed* covers both sides of the change, so
its conversation has two lines — the removal and the addition:

```
5a1e09c7  note  src/Foo.hs  awaiting agent
  ! 12 -two
  ! 12 +TWO CHANGED
  reviewer: use a named constant
```

That is one piece of feedback. Fix it once, and `reply` once to its
id.

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

`ID` is the conversation's id from `list`, or any prefix of it no other
conversation shares. The reply **appends a local post to that conversation** and
renders inline, in order and under your name, in the reviewer's live
session — this is the primary way to answer a `?`, because the answer
appears exactly where the question was asked. A reply never reaches
GitHub: on a GitHub thread it is talk beside the thread that only the
reviewer sees. Also summarize the answer in your chat reply when
you're in a conversation with the human. The thread is ordered: the
reviewer may answer your post with another of theirs, so re-check
`list` for posts newer than yours instead of assuming your
answer was the last word. Accepting the hunk (or resolving the note)
closes the whole conversation. The output says where the post landed,
`repo-wide` for a note or `on PR #N` for anything bound for GitHub. A
reply to a verdict with no note yet starts a repo-wide note.

When the answer is "look here", say it with your session pointer as well
as in the reply: `remax do :go HUNK_ID` or `:go PATH:LINE` puts them on
the code you mean (see *Pointing the reviewer*).

## Drafting review comments

```sh
remax -C /path/to/repo comment HUNK_ID "comment text"   # draft on a hunk
remax -C /path/to/repo comment --revise ID "new text"   # propose a rewrite
```

`HUNK_ID` comes from `remax hunks`. `comment` writes a GitHub comment on
the hunk's lines in the reviewer's name, marked as written by you,
pending until they submit it; they can edit or drop it first. `--revise`
records a proposal on a conversation by its id: text for a reply, or
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

## When the review moves

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

- **`-C` is a folder with `to` directly under it** (the worktree sits
  beside the story's checkouts, at the default `../<repo>-pr-<N>`):
  keep `-C`, keep the `watching` task and the state and feedback files.
  The listing spans the worktree too, and the move is reported once per
  group; find its work under the worktree's `── name ──` heading, and
  `cd` there to edit.
  If you started `watching` or a wait before the worktree existed,
  restart them: both read the folder's checkouts when they start, so
  the reviewer's header in the worktree shows nobody watching, and a
  flag filed there wakes nothing, until they are started again.
- **Otherwise**: re-point `-C` to `to`. Stop the `watching` task and
  start it again on `to`, name new state and feedback files after `to`
  (the fingerprint is per path), start the next wait there, and edit
  files there. Do the same on the way back when a later move names the
  original checkout. A wait on a path you return to may wake at once
  with the move you just followed, because that path's fingerprint is
  from before it; read the listing and wait again.

A move that enters a worktree names the pull request; the worktree is
already on its branch. Nothing is pushed or posted when the review
moves. Quitting remax writes no move: the review stays where it was
until a session moves it.

## Reading a pull request

Prefer local git to `gh`. A pull request remax has opened is already
fetched: its head is `refs/remax/pull/N/head` and its base
`refs/remax/pull/N/base`, the merge-base, so the diff between them is
the one GitHub shows. Read it as you would any branch:

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
checked out, usually in a worktree of its own (see *When the review
moves*), and that checkout's working tree is its head, holding commits
and edits not pushed yet. Read that checkout, diffed against
`refs/remax/pull/N/base`, rather than the head ref.

While a session has the pull request open, its review threads and
PR-level comments are in `remax list`, and `show` says `on PR #N` for
each of them.

Use `gh` only for what git does not hold, such as the description or a
failing check's log, and never for the diff.

## Giving a tour

When the human asks you to walk them through a change — present it,
explain it, "give me a tour" — build one and remax walks them along it.
This is the one thing you post *before* being asked about a line.

They can also ask from inside remax. A `tour` line in
`list` is that ask, and it wakes a `list --wait` like a flag does:

```
tour   feature vs b9045d7: schema changes first  (--ref feature --base b9045d7)
```

Answer it with the tour below, passing the flags in parentheses exactly
as the request gives them (`working tree` means pass no flags). The
text after the colon is their brief; follow it. Posting `tour begin` for
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
`--pr N` names a view when nobody has one open. Read the whole listing
and decide an order and a sentence or two for each stop; a stop may
cover several hunks when they are one idea (a rename and its fallout, a
signature and its call sites).

A tour of a pull request is built exactly the same way. `hunks --pr N`
lists it, local git (see *Reading a pull request*) supplies the
history, and `tour begin` and `tour step` take the same `--pr N`. Never
build one from `gh pr diff`: its hunks have no ids remax accepts. The
listing's `subject` reads `pull/N`, and the tour stays with the pull
request as its head and base move.

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

Once the last step is posted, start them on it with
`remax -C "$REPO" do :tour 1` (see *Pointing the reviewer*).

## Pointing the reviewer

When you want the reviewer looking at something particular, move their
session there instead of describing where it is:

```sh
remax -C "$REPO" do :go HUNK_ID        # an id from remax hunks, or a prefix
remax -C "$REPO" do :go src/Foo.hs:40  # a line
remax -C "$REPO" do :go CONV_ID        # a conversation from remax list
remax -C "$REPO" do :tour 2            # a stop of the tour you posted
```

When the human asks to be shown where some code is ("show me where…",
"where does…", "take me to…"), find it and run `do :go PATH:LINE` rather
than answering with a line number. Say in a sentence what they are
looking at; a reference in the chat is the fallback when `do` fails.

Point once you have something for them: the code they asked to see,
the first stop of a tour you just posted, the line your reply to their note is about, the hunk your
fix landed in. Do not point after every edit; they may have follow mode
on, and moving them while they read is an interruption.

`do` prints what the session said, such as `at the hunk in src/Foo.hs`,
or why not. It exits 1 when no remax is open on the checkout in this
qmux tab; mention the place in your reply instead. Only commands that
look, move or change the view run; anything that changes the review,
such as `:submit` or `:revert`, is refused as the reviewer's to run.
`held for the reviewer` means they are typing or have a popup open:
they will be asked to run it once they are free, so do not send it
again.

## Fixing per-commit feedback

When conversations have a commit after `@`, the reviewer reviewed commit by
commit and the fix should usually land in that commit, not at the branch
tip. Group conversations by commit and either amend surgically
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
- In a `PR #N review` view, never edit files or offer fixes: answer
  with `remax reply`, and draft comments only when asked.
- Never write to `~/.local/state/remax/review.db` directly, and never
  run `remax forget` unless the human explicitly asks.
- Post a tour only when asked for one; `tour begin` discards the
  previous tour for that view.
- No conversations and no `tour` line means no outstanding feedback,
  whatever `moved` line the listing has: a move
  you have followed stays listed. A tour request on its own is work,
  and so is a move you have not followed yet: follow it as *When the
  review moves* says.
- Under `watch`, keep one `list --wait` in flight and start a fresh one
  each time it wakes you. With none running you are only as current as
  the last time you asked. Without `watch`, do not start one at all —
  a background task the human did not ask for outlives their attention.
