# Replying and opening notes

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
the code you mean (see [Pointing the reviewer](pointing.md)).

## Opening a note yourself

```sh
remax -C /path/to/repo note src/Foo.hs:40-52 "why these lines matter"
```

`note` starts a local conversation of your own on changed lines of the
view the session last showed: a heads-up, an explanation the reviewer
asked for, or something you want them to look at before accepting. Only
lines `remax hunks` lists can carry one; lines in the range that did not
change are left out. It draws inline like a reply, waits for the
reviewer until they answer or resolve it (`list --awaiting reviewer`
shows it), and never reaches GitHub. The output names the conversation;
follow it with `remax do :go ID` when you want them there now. Open a
note when there is something to say about lines nobody asked about; to
answer a question, reply to it instead.
