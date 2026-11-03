---
name: remax-review
description: "Act on code-review feedback recorded by remax — read the reviewer's flagged/questioned lines and notes for a repository, fix them (or only answer them in a read-only pull request review), and understand how the review session sees your edits. Use when asked to address review feedback, check for remax flags, or work alongside a human reviewing in remax, or to show the reviewer a hunk, file, line or conversation by moving their remax session there (`remax do :go`), light up a few lines for them (`remax do :highlight`), or leave them a note on changed lines (`remax note`). Also use whenever the user asks to be shown where some code is: take them there instead of replying with a line number. Takes an argument: no argument answers what is outstanding once and stops; `watch` keeps waiting on the reviewer and reacting until told to stop; `stop` ends a watch."
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
