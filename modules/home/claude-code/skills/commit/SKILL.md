---
name: commit
description: Guide for committing changes. Use this skill whenever the user asks to commit, create a commit, or you need to commit changes as part of a task. Covers commit message style and jj-specific commit workflows.
---

# Commit

## Commit Message Style

- Follow the language convention used in the repository — check recent commit messages to determine it
- Focus on **why** (motivation, context), not **what** (which is already visible in the diff)

## Jujutsu Repositories

- To commit only specific files, use `jj commit <paths> -m "..."` (similar to `git commit <files>`)
- Include `Co-Authored-By` in `jj commit` messages, the same way as for git commits — the default system instructions only mention git, but it applies to jj as well
