# Reading the feedback

```sh
remax -C /path/to/repo list --awaiting agent
```

This prints what is waiting on you (run it from anywhere — `-C` points
at the target). Drop `--awaiting agent` to see every conversation,
including ones you have answered and ones with no note yet. **Three
kinds of thing can be waiting**: a `moved` line, where the review has
moved into another checkout (see [When the review moves](moves.md)), a `tour`
line, where the reviewer asked you for a guided tour of the view they
have open (see [Giving a tour](tour.md)), and conversations, each the lines a
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

## Each line's marker says which side it is

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

## When `-C` points at a folder of checkouts

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
  `remax comment --revise`, `remax comment --withdraw` and
  `remax move`: a GitHub thread's own id (`PRRT_…`) or a note's, which a
  note keeps when the reviewer grows it over more lines. The listing prints the shortest prefix no other
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
    a `PR #N review` view (see [Answering in a pull request review](pull-requests.md#answering-in-a-pull-request-review)).
  - `?` (questioned) — the reviewer wants an explanation or a fix.
    If the code is wrong, fix it, again except in a review view. If the
    code is right as written, answer the question — see [Replying to
    notes](reply.md). Never answer
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
  yet — see [A conversation with no note yet](#a-conversation-with-no-note-yet). `--awaiting agent`
  leaves out PR-level comments, review summaries, and threads a bot
  spoke last on: none of those asks you for anything.
- **where it lives** — `show` says `on PR #N` for a GitHub thread,
  PR-level comment, review or draft, and `repo-wide` for a note, which
  shows in every view that has its lines. A draft that is `repo-wide`
  was written with no pull request in view; see [Drafting review
  comments](comments.md). Every post on a conversation, yours and the reviewer's,
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

## A conversation with no note yet

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

## One flagged line, two sides

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
