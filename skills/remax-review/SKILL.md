---
name: remax-review
description: "Act on code-review feedback recorded by remax: read the reviewer's flagged and questioned lines and notes, then fix them, or only answer them in a read-only pull request review. Use when asked to address feedback the reviewer left in remax, check for remax flags, or work alongside a human reviewing in remax, or to show the reviewer a hunk, file, line or conversation by moving their remax session there (`remax do :go`), light up a few lines for them (`remax do :highlight`), or leave them a note on changed lines (`remax note`). Also use whenever the user asks to be shown where some code is: take them there instead of replying with a line number. Not for pull request comments on GitHub unless the user mentions remax or is reviewing in it; read those with gh. Takes an argument: no argument answers what is outstanding once and stops; `watch` keeps reacting to the reviewer until told to stop; `stop` ends a watch. Also use to give the reviewer a tour, including bringing them a few hunks of critical code for an early look while you keep working."
---

# Working with a remax reviewer

remax carries its own guide. Start by reading its index:

```sh
remax guide index
```

The index links to the other pages; `remax guide <page>` prints one.
Read the pages your task needs as you reach them.

The argument you were invoked with picks how long you stay, as the
index's *Answering once, or staying* describes:

- no argument: answer once;
- `watch`: stay and react;
- `stop`: end a watch.
