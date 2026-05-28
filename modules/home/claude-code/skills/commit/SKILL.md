---
name: commit
description: Guide for committing changes. Use this skill whenever the user asks to commit, create a commit, or you need to commit changes as part of a task. Covers commit message style and jj-specific commit workflows.
---

# Commit

## Commit Message Style

- Follow the language convention used in the repository — check recent commit messages to determine it
- Focus on **why** (motivation, context), not **what** (which is already visible in the diff)
- Subject is one line by default. If the why needs explaining, keep it out of the subject (drop "for ..." purpose phrases) and write it in the body, after a blank line
- Don't make the subject too specific: avoid concrete flags, options, or command names already visible in the diff
- In the body, don't restate diff-readable changes; lead with the direct trigger (what recently changed / became possible), then background — "recent change → old problem → now fix" order, not the reverse
- Use full URLs (`https://github.com/Owner/Repo/pull/123`), not GitHub abbreviations (`Owner/Repo#123`), so they stay clickable in terminals; attach them as parenthetical notes at a sentence's end rather than as its subject

## Jujutsu Repositories

- To commit only specific files, use `jj commit <paths> -m "..."` (similar to `git commit <files>`)
- Include `Co-Authored-By` in `jj commit` messages, the same way as for git commits — the default system instructions only mention git, but it applies to jj as well
