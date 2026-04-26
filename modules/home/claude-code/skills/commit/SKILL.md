---
name: commit
description: Guide for committing changes. Use this skill whenever the user asks to commit, create a commit, or you need to commit changes as part of a task. Covers VCS detection (jj vs git), commit message style, and jj-specific workflows.
---

# Commit

Run version control operations on the **host side**, not inside containers.

## Commit Message Style

- Follow the language convention used in the repository — check recent commit messages to determine it
- Focus on **why** (motivation, context), not **what** (which is already visible in the diff)

## VCS Detection

Check which version control system the repository uses before committing. If a `.jj` directory exists, the repo uses Jujutsu (jj).

## Jujutsu Repositories

In repositories with `.jj`, use `jj` commands instead of `git` for write operations:

- Use `jj commit`, `jj new`, `jj describe` — not `git commit`, `git push`
- To commit only specific files, use `jj commit <paths> -m "..."` (similar to `git commit <files>`)
- Colocated repositories (both `.jj` and `.git`) allow read-only git commands like `git log`, `git diff`, `git grep` since they are convenient and harmless
- Include `Co-Authored-By` in `jj commit` messages, the same way as for git commits — the default system instructions only mention git, but it applies to jj as well
