# remax

> [!WARNING]
> The current state of this repository is AI-generated and has not yet
> been human reviewed. Read accordingly.

Hunk-by-hunk code review with the **whole file on screen**: edit it
**in place** with helix-flavored modal keys, and record per-line,
content-keyed verdicts that survive rebases. Runs in a plain terminal (zellij-friendly).

```sh
remax                 # review everything uncommitted (working tree vs HEAD)
remax --base REF      # review the working tree against any ref
remax --branch        # against the merge-base with the mainline (whole branch)
remax --unstaged      # working tree vs index
remax --untracked     # new files, reviewed as pure additions
remax forget          # forget verdicts under the current directory (asks first)
remax list [--json]   # flagged/questioned punch list for this repository
remax reply KEY TEXT  # agent answers a note; shows live in the session
remax -C DIR ...      # any of the above, as if started in DIR
```

The buffer on screen is always the working file — editing edits the real
file — so the mode picks the *base* the diff decorates against. (There is
deliberately no index-vs-HEAD mode: with a directly editable buffer,
reviewing an index that differs from the worktree isn't coherent.)

## Persistent by design

remax is built to be left open all day in a pane. The change set follows
your git activity on its own: commit from your shell and the reviewed
hunks leave the screen; when everything is settled the session shows an
"all clear" idle screen instead of exiting, and the next edit anywhere in
the repository wakes it. The CLI flags above are just starting values —
`:base`, `:mode`, and friends switch the view from inside. Verdicts are
keyed by line content, so they survive commits, rebases, and reloads; a
line that changes after being reviewed comes back as pending by itself.

## The screen

- Header: mode, current file, file/hunk position, pending count.
- Tray (left, `Tab` collapses): hunks grouped by file, with per-hunk
  marks — `·` pending, `✓` reviewed, `!` flagged, `?` questioned. The
  tool starts in **pending-only** mode: fully *reviewed* hunks fold under
  their file line as a dim `·N✓` count and `]`/`[` skip them; flagged and
  questioned hunks still count as pending — they need another look — so
  they stay listed and navigable. `Space p` switches to showing (and walking)
  every hunk. The current hunk always shows either
  way. `{` / `}` scroll the tray; it auto-centers on the current hunk
  when you jump. Long paths compress like `s/R/Quote.hs` so the file
  name always survives.
- The tray/body divider (thin `▎`, in the darker `ui.linenr` gray) runs
  the full pane height and doubles as the current-hunk indicator: across
  the current hunk's rows it widens to a half-block `▌` in the highlight
  color, so the hunk's extent reads at a glance. Next to it, a status
  column (cursor `▌`, `!` flagged, `?` questioned, `·` reviewed) and
  absolute line numbers — position and review state are always both
  visible. Removed lines have no number, keeping the numbering honestly
  absolute.
- Body: the whole file, scrollable, syntax-highlighted by **tree-sitter**
  with **your helix theme** (the `theme` from `~/.config/helix/config.toml`,
  `inherits` followed, palette resolved; a built-in scope palette covers
  users without helix). The jellybeans base theme is embedded in the
  binary, so an `inherits = "jellybeans"` chain resolves even with no
  helix runtime on disk (AppImage installs mount theirs only while helix
  runs); other parents are found via `$HELIX_RUNTIME` or the usual helix
  theme directories. Colors render as exact 24-bit values when
  `COLORTERM` says the terminal can (the same signal helix trusts) —
  otherwise the terminal's palette approximation applies. Hunks are decorated in place by a live diff
  against the base: removed lines float inline, and reviewed lines keep
  their syntax colors under a dim style (plus a gutter mark), so the
  unreviewed lines still pop when a hunk comes back — without flattening
  the reviewed code to gray.
- Status line: mode chip (NOR/INS), messages, key hints — styled by your
  `ui.statusline` theme scopes.

## Keys

Normal mode:

| key | action |
|---|---|
| `h j k l` / arrows | move (collapses the selection, helix-style) |
| `w b e` | word motions — **the crossed span becomes the selection**, so `wc` is "change word", `ed` is "delete to word end" |
| `f F t T` + char | find / till a character (forward / back, across lines) — the crossed span becomes the selection |
| `gh gl gs` / Home End | line start / line end / first non-blank — on a long line the view scrolls horizontally to follow the cursor (gutter stays pinned); `gh` brings it home |
| `gg ge` | file start / file end |
| `v` | extend mode: every motion grows the selection; `;` collapses |
| `x` | select line, repeat to extend |
| `X` | select the current hunk — the review-unit object: `X r` reviews what you just saw selected, `X d` deletes it, `X >` indents it |
| `%` | select the whole file |
| `d` / `c` | delete / change the selection (char-wise or line-wise; or the char under the cursor) |
| `i a I A o O` | enter insert — editing is just editing |
| `~` | toggle case of the selection (or the char under the cursor) |
| `>` / `<` | indent / unindent the selected lines (2 spaces; blank lines untouched; selection survives for repeats) |
| `J` | join the selected lines (or current + next), comment-aware: joined lines shed their own comment leaders |
| `Q` | reflow to 80 columns, comment-aware: the selection, or the same-prefixed paragraph around the cursor, keeping indentation + comment leader (`--`, `-- \|`, `//`, `#`, `;;`…) on every line |
| `r` + char | replace every character of the selection (or under the cursor) with that character, helix-style |
| `u` | undo |
| `y` | yank the selection (or cursor cell) — line-wise for `x` selections |
| `p` / `P` | paste after / before: whole lines below/above for a line-wise yank, after/at the cursor otherwise; the paste is selected |
| `Space` | menu (helix-style infobox): `f` file picker, `b` open files, `n` note popup, `e` `$EDITOR` here, `p` pending filter |
| `.` | repeat the last insert session (the entering command plus what was typed), helix-style — `c`-fix-`Esc` then `.` applies the same change at the cursor |
| `Enter` | **accept the selection as reviewed** — a selection is required, so what you accept is always visibly highlighted (`X Enter` for the hunk you're looking at). Completing a hunk jumps to the next pending one; a partial mark steps to the next line |
| `!` / `?` | flag-to-return-to / question — the selection's lines, or the whole current hunk with none. The note popup opens immediately for the reason (`note-on-flag = 0` in the config turns that off); discarding keeps the mark |
| `]` / `[` | next / previous hunk, crossing file boundaries |
| `/`, `n` / `N` | search the file (literal, helix-shaped: the match becomes the selection); repeat forward / back, wrapping |
| `:` | the command line — see below |
| `*` | search for the selection (single-line) or the identifier under the cursor — primes `n`/`N` and jumps to the next occurrence |
| `'` | jump back to the current hunk's anchor |
| `E` | suspend and open `$EDITOR` (hx-aware: helix gets `file:line`, classic editors get `+line`) on the current file at the cursor's line; the session reloads whatever you changed when you return |
| `)` / `(` | next / previous file |
| `Ctrl-p` | fuzzy file finder over the whole repository (tracked + untracked); type a subsequence, Enter opens, Esc closes |
| `Tab` | collapse / expand the tray |
| `{` / `}` | scroll the tray |
| `q` | quit (prints the pass summary and punch list) |

During a rebase the status line and header carry the rebase state; all
normal review keys keep working at every stop.

Jumping to a hunk parks the cursor on its first line still needing
attention, and a session opens on the first hunk nothing has been said
about yet, and auto-advance walks the same way: fresh hunks first,
circling back to the flagged and questioned ones only when nothing
unvisited remains. Splitting never severs a block comment — a blank
line inside `{- -}`, `/* */`, or `<!-- -->` is prose, not a boundary,
and cut points avoid the middle of comment runs. Oversized hunks (>20 changed lines) split automatically, in
three tiers: always at function boundaries (a blank changed line
resuming at column 0) — and never re-merged across one, however small
the functions; within a function, anything over `piece-max` lines splits
at its interior blank lines, with pieces under `piece-min` merging
forward; a blankless slab still over the cap cuts at its least-indented
line. All three knobs live in the config file. Content-keyed
verdicts are indifferent to where the boundaries fall.

Insert mode: type, Enter, Backspace, Del, arrows, paste; `Esc` returns to
normal mode and **writes the file to disk**. Normal-mode edits that don't
enter insert (`d`, undo) write immediately; ones that do (`c`, `o`)
write when insert exits. There is no explicit save and no dirty state in
normal mode — what you see in normal mode is what's on disk.

## Emacs keys

`keys = emacs` in the config (or `:set keys emacs`, live) swaps the
buffer to modeless emacs editing: printable keys self-insert and write
through on every edit. `C-f/b/n/p` and arrows move, `C-a`/`C-e` line
ends, `M-f`/`M-b` words, `M-<`/`M->` file ends; `C-space` sets the mark
and the region is the selection the review verbs consume; `C-w`/`M-w`
kill/copy the region, `C-k` kills to end of line, `C-y` yanks,
`C-_` (aka `C-/`) undoes, `C-g`/`Esc` clears the mark. `M-x` opens the
command line, `C-s` the search prompt. Review verbs live behind the
`C-c` prefix with the same letters as the helix keys — `C-c Enter`
accept, `C-c !` / `C-c ?` flag/question, `C-c x` select hunk,
`C-c ]`/`C-c [` hunks, `C-c p` pending filter, `C-c f` files — and
`C-x C-c` quits, `C-x C-s` saves (edits already write through). The
note popup keeps its modal keys in either keymap.

## The : command line

| command | action |
|---|---|
| `:base REF` | review the working tree against REF (any commit-ish) |
| `:base branch` / `:branch` | against the merge-base with the mainline — the whole branch diff, fixes land at the tip |
| `:mode head` / `:mode unstaged` / `:mode untracked` | the classic change sets |
| `:rebase [REF]` | start a commit-by-commit review pass (below) |
| `:continue` | fold this stop's edits into the stopped commit and advance |
| `:abort!` | abort the rebase (`:abort` asks first) |
| `:amend` | outside a rebase: fold the session's edits into HEAD (pairs with `:base HEAD^`) |
| `:note TEXT` | attach a one-line note to the lines just marked `!`/`?` (or the selection) |
| `:note` | open the note popup — a small modal buffer (full editing keys) for richer notes; text auto-wraps at the popup width; `⏎` in its normal mode saves, `Ctrl-c` (any mode) or `Esc` in its normal mode discards, saving empty clears. With the cursor on a noted line, `:note` opens that note for editing |
| `:unnote` | remove the note from the same target |
| `:accept PATH` | mark every hunk under a directory (or in one file) reviewed — the bulk verb for generated code; matching is by path component |
| `:open PATH` / `:close` | open any repository file in the session / drop it again |
| `:files` | the fuzzy file finder (also `Ctrl-p`) |
| `:edit [PATH]` | suspend into `$EDITOR` at the cursor (or on PATH) |
| `:q` | quit |

The command line completes itself: a bare `:` lists every command with
a one-line description, typing narrows the menu, and `Tab` /
`Shift-Tab` cycle completions — command names first, then that
command's arguments (refs for `:base`/`:rebase`, files for
`:open`/`:edit`, directories for `:accept`, modes for `:mode`).

A file opened by hand arrives with zero hunks (it reviews against its
own content at open time), stays pinned across git refreshes, and any
edits you make to it decorate as hunks like everything else. Notes
render inline — a dim `└ ≡ …` footnote under the block they annotate —
and hunks carrying one show `≡` in the tray.

Every base or mode switch closes the current review pass and opens a new
one, so pass history records what was reviewed against what.

## Reviewing a branch commit by commit

`:rebase` (optionally `:rebase REF`) runs `git rebase --exec false` from
the merge-base: the failing exec pauses the sequencer after **every**
commit, and the session follows along — the header shows
`rebasing 2/5: <subject>`, the hunks are that commit's changes, and any
edit you make lands in *that commit* when `:continue` amends and
advances. A conflict stop is labeled `CONFLICT`; resolve it in the buffer
and `:continue`. This is the surgical alternative to the ambient
`:base branch` view (where fixes land at the branch tip).

The session never runs git on its own — those five commands are the only
mutation points, each one explicit. The awareness half works with zero
commands too: drive `git rebase --exec false`, `--continue`, `--abort`
from your shell and the open session tracks every stop.

## Working with agents

The review loop closes with coding agents. An agent's edits stream into
your open session live (files reload, touched lines return to pending);
your feedback flows back through the store:

1. Mark a line or hunk `!` (fix this) or `?` (explain or fix), and give
   the reason: `:note use mapMaybe here instead`.
2. The agent runs `remax -C /path/to/repo list --json` and gets file,
   verdict, exact line text, position, and your note. Feedback given
   during a `:rebase` stop also carries the commit it is about
   (sha + subject — final values, since stops only rewrite forward), so
   an agent can land fixes in the introducing commits; ambient feedback
   carries no commit and agents recover one from the line text with
   `git blame` when it matters.
3. For a `?` it can answer in place: `remax reply KEY "text"` (the key
   comes from the JSON) attaches an answer that renders live under your
   note in the open session, marked `↪`. Rewriting or removing your
   note clears the stale reply.
4. It fixes the code; the fixed lines automatically come back as pending
   in your session; you accept or push back.

`skills/remax-review/SKILL.md` teaches Claude Code this protocol — copy
or symlink it into `~/.claude/skills/` (or a workspace `.claude/skills/`)
so agents pick it up.

## Configuration

`~/.config/remax/config.toml` — flat `key = number` lines, `#` comments,
every key optional (defaults shown):

```toml
scrolloff = 5         # rows kept visible around the cursor
note-wrap = 58        # auto-fill column in the note popup
note-on-flag = 1      # ! and ? open the note popup immediately
split-threshold = 20  # hunks at or under this never split
piece-min = 6         # smaller pieces merge forward (within a function)
piece-max = 32        # bigger pieces keep splitting
```

`keys = helix` (default) or `keys = emacs` picks the keybinding family.
Every setting can also be changed live from inside the session:
`:set` lists current values, `:set KEY` shows one, and `:set KEY VALUE`
applies immediately (split knobs re-split on the spot) and persists to
the config file. Unknown keys and unparsable lines are ignored. A `.remaxignore` at the
repository root (one pattern per line, `#` comments) keeps files out of
the change set entirely: a path prefix matched by component (`gen`
covers `gen/a.txt`, not `generated.hs`) or a `*suffix` form
(`*.min.js`). Check it in — generated trees are a team-wide fact.
Ignored files can still be `:open`ed. The header's pending
count reads `N pending here · M overall` — per current file, and across
the whole change set.

## How review state works

Each changed line is keyed by content — repository, file, the line text
with its `+`/`-` marker, and a copy ordinal for identical twins — under
the `remax/1` scheme.
Keys carry no positions, so verdicts survive line shifts, rebases, and
hunk-boundary changes. A hunk is pending while any of its lines lacks a
verdict; add one line to a reviewed hunk and it comes back with the old
lines dimmed.

Verdicts live in `~/.local/state/remax/review.db` (sqlite): `line_verdict`
rows (key, file, line text, verdict, pass) plus a `pass` table recording
when each review ran and what scope it covered.

## Filesystem awareness

A watcher on the repository keeps the screen honest while you work
elsewhere (helix, git, a build):

- a watched file changed on disk → reloaded in place, decorations rebuilt
  (if you're mid-insert, your edit wins on Esc and a warning shows)
- `.git` index/HEAD/refs changed → the change set is re-listed: new files
  appear, vanished ones drop out, changed bases re-diff (throttled to
  twice a second; `dist-newstyle`/`.stack-work`/`node_modules` ignored)

## Performance

Startup watches only the directories holding changed files plus `.git`
(never a recursive walk over build artifacts), and syntax highlighting is
computed lazily — a file gets its real colors the moment it is first
shown, never sooner. Decorations are cached per file and rebuilt only on
text change; while typing, only the edited line is re-tokenized.
Rendering materializes only ±300 rows around the cursor. The diff runs
under a 400ms budget (a heavily rewritten file degrades to one
whole-file hunk instead of hanging), tokenization under 800ms (degrades
to plain text), and files over 10k lines render plain outright — they
are almost always generated. The text buffer is a purpose-built line
zipper (Remax.Zipper): the off-the-shelf one constructed its state
character by character, quadratic in line count. Measured: ~50ms to first frame on a small
change set; ~1.5s on a 35-file set with several 4k-line files inside a
20k-directory repo.

One trade from selective watching: a file in a directory containing no
changed files won't fire a watcher event when edited; its entry into the
change set is noticed at the next git-state event instead.

## Highlighting

Tree-sitter, fully self-contained: the MIT runtime and ten grammars are
compiled into the binary (haskell, purescript, yaml, json, toml,
hcl/terraform, sql, bash, nu, markdown — all MIT except hcl's
Apache-2.0), with helix's highlight queries embedded (MPL-2.0,
file-level; see vendor/grammars/QUERY-LICENSE.md). Query predicates
(#eq?, #any-of?, and #match? in its anchored-alternation form) are
evaluated; painting follows the innermost-node-wins rule. Markdown
covers block structure (headings, lists, quotes, fences); inline
emphasis and links live in a separate injection grammar not yet wired. No system
tree-sitter, no helix installation, and **no GPL anywhere in the
dependency tree**. Languages without a vendored grammar render plain —
vendoring another is copying its sources under vendor/grammars and adding
one table entry.

## Development

The repo follows the flipstone house layout: `./scripts/bootstrap` once
(creates the shared stack volume and `.env`), then `./scripts/test`
builds in the `haskell-tools` dev container (`compose.yaml`), and
`./scripts/build` formats with fourmolu first. `package.yaml` is the
source of truth — `remax.cabal` is hpack-generated. Plain `cabal build`
also works outside the container.

## Known v1 edges

- Word motions are line-local; the helix subset is deliberately small
  ("finer points to be negotiated").
- Files without a trailing newline gain one on first write.
- The line being edited shows plain (uncolored) while insert mode is
  active; colors return on Esc when the full tree-sitter pass reruns.
- Binary files are skipped.
