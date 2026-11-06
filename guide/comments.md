# Drafting review comments

```sh
remax -C /path/to/repo comment HUNK_ID "comment text"   # draft on a hunk
remax -C /path/to/repo comment --revise ID "new text"   # propose a rewrite
remax -C /path/to/repo comment --withdraw ID            # drop your proposal
```

`HUNK_ID` comes from `remax hunks`. `comment` writes a GitHub comment on
the hunk's lines in the reviewer's name, marked as written by you,
pending until they submit it; they can edit or drop it first. `--revise`
records a proposal on a conversation by its id: text for a reply, or
for a draft of theirs. Nothing happens until the reviewer takes it with
Enter, which makes it their own pending text, and a new proposal
replaces the old. `--withdraw` drops your proposal on a conversation
until the reviewer takes it. On a comment you drafted on a hunk, the
conversation goes with it, local talk included, and the output counts
the local posts it dropped. Once taken, the text is the reviewer's and
withdrawing refuses. Draft or revise only when asked to.

A draft belongs to the pull request it was written for, and the output
names it with the draft's id: `comment 3f9c… proposed on src/Foo.hs:42 on
PR #482`. A draft written with no pull request in view belongs to none
and shows in every pull request until it is submitted or moved. When a
pull request is closed and reopened under a new number, its drafts stay
with the old one; move each draft, with every post on it, when the
reviewer asks:

```sh
remax -C /path/to/repo move DRAFT_ID --pr 512
```

Only drafts move. A GitHub thread, PR-level comment or review lives on
the pull request GitHub keeps it on, and a note is repo-wide.

The session writes each poll of the pull request to a cache file that
`list` reads, so GitHub threads appear in the listing while a session
has the pull request open, and a watch prints a new GitHub post
as it does a new note.
