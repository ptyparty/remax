# Giving a tour

When the human asks you to walk them through a change — present it,
explain it, "give me a tour" — build one and remax walks them along it.
This is the one thing you post *before* being asked about a line.

They can also ask from inside remax. A `tour` line in
`list` is that ask, and a watch prints it like a flag:

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
The top level is an array with one object per repository in reach
(one, unless `-C` names a directory of several checkouts), and the
hunks sit in that object's `hunks` array:

```json
[
  {
    "repo": "/home/me/src/shop",
    "subject": "feature",
    "view": "feature vs 1a2b3c4d5e",
    "hunks": [
      {
        "id": "9f2c41d07a…",
        "file": "src/Cart.hs",
        "line_no": 42,
        "lines": ["-total = sum prices", "+total = sum (map price items)"]
      }
    ]
  }
]
```

So `jq -r '.[].hunks[] | "\(.id)\t\(.file):\(.line_no)"'` lists one hunk
per line. `id` is the long hex string `tour step` takes; there is no
other key to look a hunk up by. `remax hunks --help` and every other
subcommand's `--help` print their usage.

With no flags it follows **the view their session last had up** — the
working tree, a `:ref`, a `:commit` — so you do not need to be told
which commit they have up while they are looking at it. Read the whole listing
and decide an order and a sentence or two for each stop; a stop may
cover several hunks when they are one idea (a rename and its fallout, a
signature and its call sites).

A tour of a pull request is built exactly the same way. `hunks --pr N`
lists it, local git (see [Reading a pull request](pull-requests.md#reading-a-pull-request)) supplies the
history, and `tour begin` and `tour step` take the same `--pr N`. Never
build one from `gh pr diff`: its hunks have no ids remax accepts. The
listing's `subject` reads `pull/N`, and the tour stays with the pull
request as its head and base move.

## Preparing a tour ahead of time

When the reviewer will look later, or has not said which view, name the
view yourself on `hunks`, `tour begin` and every `tour step`, because
"the view their session last had up" may be anything by then:

- committed work on a branch: `--ref BRANCH` alone. remax compares it
  with the commit where it forked from the mainline, so only the
  branch's own changes show.
- a branch cut from another branch than the mainline: `--ref BRANCH
  --base PARENT`, naming the parent branch. A branch name as `--base`
  means where `BRANCH` forked from it, never that branch's tip.
- a single commit: `--ref SHA --base SHA^`
- a pull request: `--pr N`
- uncommitted changes: `--base HEAD`

The base must be where the work forked. Never pass the tip of a branch
that moves, such as `$(git rev-parse origin/main)`, as `--base`: a sha is
taken as it is, and everything that landed on main since the branch
was cut would show up in the tour, reversed, as if the branch had
undone it. `hunks` and `tour begin` print the base they used (`base
1a2b3c4d5e, where feature forked from origin/main`) and warn when a
`--base` is not an ancestor of `--ref`; read that line before posting
steps.

Give the tour a name with `tour begin --name WORDS`, a few words that
say what it walks. The reviewer finds it with `:tours` (or Space T when
the view they have open has no tour): it lists every tour kept for the
repository by name, view and age, and picking one opens that view and
starts the tour at its first stop. Tell them the name rather than the
flags. `remax -C "$REPO" tour list` prints the same list, so check the
tour is there before you say it is ready. `remax -C "$REPO" do ':tour
show NAME'` opens it in the reviewer's session, switching to its
repository and view.

## An early look at critical code

You need not wait until the work is done, or be asked. When you reach
code where a wrong turn would be costly to undo, such as a new data
model or schema, a public interface or wire format, a tricky algorithm,
or code that moves money or guards access, bring the reviewer just
those hunks and keep working:

```sh
remax -C "$REPO" snapshot "caching model"
```

`snapshot` keeps the working tree as it is, untracked files included,
as a commit under `refs/remax/snapshot/`, without touching the working
tree, the index or the branch. It prints the `--ref` and `--base` to
give `hunks`, `tour begin` and `tour step`, so the tour holds still
while you go on editing. Snapshotting again under the same name moves
it; post the tour again after that. Reuse one name for one piece of
work rather than making a new snapshot each time: the reviewer's
`remax forget` on the repository clears its tours and their snapshots.

Keep an early tour to the few hunks that decide the design, two to five
stops, and use the first note to say what you want judged and what is
still rough. Then carry on as if the design holds, with `remax watch`
running (see [Staying and reacting](watch.md)) so their notes reach you
as they write them. Fold each note in and reply saying what changed. If
a note overturns the design, rework it before building further on it.
Do not stop to wait for the review: tell the reviewer the tour's name in
a sentence, or post it as their move where they track work.

## Posting the stops

```sh
remax -C "$REPO" tour begin --name "caching toggle"
remax -C "$REPO" tour step --title "the new table" ID "Start here; everything else hangs off it"
remax -C "$REPO" tour step --title "call sites" ID,ID,ID "The three call sites the signature change touched"
```

`--title` is a few words naming the stop. Every hunk in the stop gets a
header reading `stop 3/12 · call sites`, so a reviewer who lands on
one of its hunks out of order still knows where they are; the note
itself shows only on the first hunk. Give every stop a title when the
tour has more than a handful.

`tour begin` empties any earlier tour for that view and names the new
one. Each `tour step` appends
one stop and says how many of the view's hunks the tour now covers and
how many it leaves out:

```
step 12: 2 hunks
the tour covers 81 of the view's 569 hunks; 488 left out
```

A tour need not cover every hunk, but leaving some out is a choice to
tell the reviewer about: when the last step still leaves hunks out, say
in the tour's first note or in your message which ones and why
(generated files, mechanical renames). The reviewer's session shows the
same count when the tour starts and in its stops list.

The reviewer's session switches to following the
tour as the first one lands and each note renders above its hunk on
its own background. An id that is not in the view is refused, so re-run
`hunks` after you edit.

Keep a stop small: one hunk, or the two or three that are one idea.
`]`/`[` walk the hunks inside a stop one by one and the header counts
both the stop and the position within it, so a twenty-hunk stop reads
as twenty hunks with one note at the front. Split by idea instead.

Finish a function before you leave it. When a function's change spans
several hunks, take them in order, in one stop or in stops back to back,
before moving to another file or another part of the file. A reviewer
who has read half a function and then lands somewhere else loses the
thread. If the order of ideas really needs a detour, say so in the
note where you leave ("the rest of `load` is stop 7") and pick it up
where you said.

Keep notes short and pointed at the code — what to look at and why it
is there, not a restatement of the diff. The reviewer still reads every
line; the tour is the order and the context, not the review. Do not
turn feedback into tour steps: a `!` or `?` is theirs to give.

Once the last step is posted, start them on it with
`remax -C "$REPO" do :tour 1` when their session is on the tour's view
(see [Pointing the reviewer](pointing.md)). When it is on another view,
or closed, `do :tours` opens the list for them, or tell them the tour's
name.

## Walking the reviewer along

When the reviewer asks to be walked through the tour on a timer, start
it in their session rather than looping over `do :go` yourself:

```sh
remax -C "$REPO" do ':tour play'      # longer on bigger hunks
remax -C "$REPO" do ':tour play 4'    # four seconds on each
remax -C "$REPO" do ':tour stop'
```

`:tour play` visits the tour's hunks that have no verdict yet, from
where their cursor is. Esc or q in remax stops it at once and the status
line says where it stopped, so they never have to come back to you to
say stop. Do not run a loop of your own that moves their session: they
cannot stop it from remax.
