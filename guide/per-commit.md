# Fixing per-commit feedback

When conversations have a commit after `@`, the reviewer reviewed commit by
commit and the fix should usually land in that commit, not at the branch
tip. Group conversations by commit and either amend surgically
(`git rebase --exec false <base>`, fix at each relevant stop,
`git commit -a --amend --no-edit`, `git rebase --continue`) or make
fixup commits (`git commit --fixup <sha>` per group, then
`git rebase --autosquash <base>`). If history rewriting is risky in
context (shared branch), ask the human before rewriting; fixing at the
tip is the safe fallback.
