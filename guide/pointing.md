# Pointing the reviewer

When you want the reviewer looking at something particular, move their
session there instead of describing where it is:

```sh
remax -C "$REPO" do :go HUNK_ID        # an id from remax hunks, or a prefix
remax -C "$REPO" do :go src/Foo.hs:40  # a line
remax -C "$REPO" do :highlight src/Foo.hs:40-52  # these lines, lit up
remax -C "$REPO" do :go CONV_ID        # a conversation from remax list
remax -C "$REPO" do :tour 2            # a stop of the tour you posted
```

When the human asks to be shown where some code is ("show me where…",
"where does…", "take me to…"), find it and run `do :go PATH:LINE` rather
than answering with a line number. When what you mean is a few lines
rather than a place, run `do :highlight PATH:FIRST-LAST` instead: it
moves them there and lights those lines up until they leave the file.
Say in a sentence what they are looking at; a reference in the chat is
the fallback when `do` fails.

Point once you have something for them: the code they asked to see,
the first stop of a tour you just posted, the line your reply to their
note is about, the hunk your fix landed in. Do not point after every
edit; they may have follow mode on, and moving them while they read is
an interruption.

Pick the pointer by how long what you say should last. A highlight is
gone once they leave the file: use it for "these lines" while you talk
in the chat. When the explanation belongs with the code and should wait
for them, open a note (see [Opening a note yourself](reply.md#opening-a-note-yourself)) and send them to
it with `do :go` and the id the note printed.

Run `do` with `-C` on the repository the place is in, even when the
reviewer is looking at another one. A session opened on a story
directory switches to that repository first, as `:repo` would, and runs
the command once it has loaded; `do` waits for that.

`do` prints what the session said, such as `at the hunk in src/Foo.hs`,
or why not. It exits 1 when no remax is open on the checkout or its
story in this komux tab; mention the place in your reply instead. Only
commands that look, move or change the view run; anything that changes
the review, such as `:submit` or `:revert`, is refused as the
reviewer's to run.
`held for the reviewer` means they are typing or have a popup open:
they will be asked to run it once they are free, so do not send it
again.
