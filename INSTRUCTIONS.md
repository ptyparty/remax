# Instructions

- Make a port of the `dev review` command found in `~/dotfiles/bin/dev` that has full
  editing capabilities

# Requirements

- I want to be able to review code hunk by hunk in a way where was I've reviewed is
  tracked a  la the `dev review` command

- I also want to be able to edit said code inline without launching a separate editor

- Syntax highlighting would be a plus

- This must run in a terminal so I can detach and re-attach via a multiplexer
  - I use zellij currently

- I want to be able to navigate amongst hunks freely and have some sense of what
  I have reviewed and what I haven't

- When looking at a hunk I want to clearly understand what file it exists in

- Respond to filesystem events by updating the current file on screen and the state
  of hunks overall.

- Show a (collapsible) tray of hunks to review, grouped by file name

- Support basic helix-style editing
  - Finer points to be negotiated
  - This includes movements leading to selection so that insert, change, yank, replace, etc work
  - Assume multiple cursors is _not_ required for now

- Match my existing helix syntax highlighting as closely as is reasonable

- Don't put specific references to these requirement bullet points in the code comments

- Allow me to mark a hunk as to be returned to. I can leave comment trails in the code

- Allow me to search in the file using / (a la helix)

- Allow me return to the currently selected hunk via a key

- Support reviewing different change sets (all uncommitted, unstaged only, untracked files)

- Keep review state in a persistent store, plus a history of review passes

- Let me list flagged lines later and forget recorded state for files under a directory

- Stay responsive on large files and large repositories

- Shareable with other developers, who mostly don't use helix — the tool must be fully useful without helix installed

- No GPL-encumbered dependencies in the distributed tool

- Eventually offer vi-mode and emacs-mode keybindings alongside helix-style

- I must have a way to highlight and mark specific lines as reviewed rather
  than a whole chunk

- I must have a way to unmark a hunk that was erroneously marked

- I need a way to navigate just unresolved hunks in addition to
  all hunks
  - Maybe a hide/show resolve hunks rather than separate navigation keys?

- Must be able to nagivate between files under review (without navingating through every hunk)

- The persistent, always-open session is the primary mode of use: never exit
  when the change set empties, and allow switching the review base and mode
  from inside the session

- Support commit-by-commit branch review where fixes land in the commit that
  introduced them

- Make review feedback readable by coding agents (machine-readable punch list
  with reviewer notes), and runnable against a repo from another directory

- Repeat the last edit with `.` a la helix

- Open files outside the change set in the session, including via a fuzzy
  file finder

- Launch the full editor on the current file with the cursor on the same line

- Review, edit, and remove notes in-session; richer notes edited in a popup
  rather than the command prompt

- Jump to the first unaccepted line of a hunk; split large hunks
  automatically so whole modules aren't one approval

- Bulk-accept everything under a directory (generated code), and support a
  per-repo ignore file that keeps generated paths out of the change set
