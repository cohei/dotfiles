---
name: jujutsu
description: Conventions for a Jujutsu (jj) repository, recognized by a `.jj` directory; check for it before the first version control command in any repository, and invoke this skill before any git or jj command there.
---

# Jujutsu

## Colocated with Git

- A `.git` directory alongside `.jj` means the repository is colocated: use git read-only, for what jj lacks (`git grep`); never `git status` or `git rev-parse`, which misread a jj working copy

## Committing

- To commit only specific files, use `jj commit <paths> --message "..."` (similar to `git commit <files>`)
- `--message` takes a single value; unlike git, repeated `--message` don't form paragraphs (the last one wins). For a subject plus a `Co-Authored-By` trailer, pass one `--message` with an embedded blank line

## Amending a Commit

- Don't `jj edit` a finished commit to amend it: work in a fresh `jj new` working copy and `jj squash` it in. `jj describe <rev>` for a message-only change is fine

## Conflicts

- A rewrite succeeds even when it leaves conflicts in descendants; that is the design, not a reason for `jj undo`
