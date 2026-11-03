# Giving a tour

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
lists it, local git (see [Reading a pull request](pull-requests.md#reading-a-pull-request)) supplies the
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
`remax -C "$REPO" do :tour 1` (see [Pointing the reviewer](pointing.md)).
