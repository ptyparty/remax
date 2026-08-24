# Platform decision — Phase A gate

Two spikes, both working as of 2026-08-21, both reworked after the first
feel test ("show the whole file; review in the context of the normal
editor"): brick is now a whole-file view with live-diff hunk decorations;
steel is now a HUD over plain helix with the hunk as a real selection.
Feel-test each and record the verdict here (the store design in the plan
proceeds either way).

Try them (a demo repo with staged changes lives at /tmp/remax-demo):

- **Spike 1 — standalone Haskell**: run `remax-spike` in any repo with
  staged changes (`--unstaged` for the working tree). Whole file on
  screen, hunks decorated in place, edit anywhere; edits go to scratch
  copies under /tmp/remax-spike. See spikes/brick/README.md.
- **Spike 2 — inside helix (Steel fork)**: run `hx-steel`, then
  `:remax-review`. A HUD pill appears; A-n/A-p/A-r drive review while
  everything else stays normal helix. See spikes/steel/README.md.

## Criteria (from the plan)

| criterion | brick | steel |
|---|---|---|
| Editing feel (built-in modal subset vs actual helix) | | |
| Hunk awareness in file context (inline old side vs native selection) | | |
| Keybinding fidelity to helix muscle memory | | |
| Syntax highlighting quality | | |
| Platform risk (stock toolchain vs experimental fork daily-driven) | | |
| Maintenance story | | |

## Verdict

**Standalone Haskell TUI (brick).** Decided 2026-08-21 after feel-testing
both v2/v3 spikes: "it feels like we're getting closer to what I want
faster with the haskell tui than the helix plugin."

The Haskell tool is built at the repo root as the `remax` package.

The `spikes/` directory (both the brick prototype and the Steel fork
clone), the `hx-steel` launcher, and the `.scm` stubs in ~/.config/helix
were deleted in August 2026 once the tool stopped depending on any of
them — the last tie was theme discovery, cut by vendoring jellybeans
into `vendor/themes/` (embedded in the binary). The Steel-spike findings
(HUD-over-editor, native selections, inlay-hint virtual minus lines)
live on only in this file's history and upstream; any future grammar or
theme vendoring pulls from the upstream repositories directly.
