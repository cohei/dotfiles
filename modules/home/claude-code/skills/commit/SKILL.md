---
name: commit
description: Guide for committing changes and writing commit messages. Invoke BEFORE running any `git commit`, `jj commit`, or `jj describe`, or otherwise writing, amending, or rewriting a commit message — including commits you initiate yourself as part of a larger task, not only when the user explicitly asks to commit. Covers commit message style and jj-specific commit workflows.
---

# Commit

## Confirm the Message

Present the drafted message and stop. Run the commit only after the user approves it; never draft and commit in the same turn.

## Commit Message Style

- Follow the language convention used in the repository — check recent commit messages to determine it
- Focus on **why** (motivation, context), not **what**, since the diff already shows what changed; don't restate any of it, including a detail it carries such as one in an added code comment
- Use full URLs (`https://github.com/Owner/Repo/pull/123`), not GitHub abbreviations (`Owner/Repo#123`), so they stay clickable in terminals; attach them as parenthetical notes at a sentence's end rather than as its subject
- Wrap code identifiers (option names, flags, commands) in backticks

### Subject

- One line by default; if the why needs explaining, keep it out of the subject (drop "for ..." purpose phrases) and write it in the body, after a blank line
- Don't make it too specific: avoid concrete flags, options, or command names already visible in the diff
- Mark work-in-progress commits with a `[WIP]` suffix in the subject

### Body

- Keep it minimal: carry only the core why, dropping secondary justifications
- When the why needs unpacking, lead with the direct trigger (what recently changed / became possible), then background, in "recent change → old problem → now fix" order, not the reverse

## Jujutsu Repositories

- To commit only specific files, use `jj commit <paths> -m "..."` (similar to `git commit <files>`)
- Include `Co-Authored-By` in `jj commit` messages, the same way as for git commits — the default system instructions only mention git, but it applies to jj as well
- `-m/--message` takes a single value; unlike git, repeated `-m` don't form paragraphs (the last one wins). For a subject plus a `Co-Authored-By` trailer, pass one `-m` with an embedded blank line
