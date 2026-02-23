# User Memory

## Version Control

- Run version control operations on the **host side** (not inside Docker Compose)

### Jujutsu

Some repositories use **Jujutsu (jj)** instead of git. Check which one is used.

In Jujutsu projects:

- Use `jj` commands instead of `git` commands by default
- Colocated repositories have both `.jj` and `.git`, so convenient git commands like `git grep` are also acceptable
- Default instructions only mention `Co-Authored-By` for git commits, but include it in `jj commit` messages as well

## Editing Style

When editing files, please follow these principles:

- **Always preserve existing correct information**
- Update = add/modify, not replace
- Only delete when explicitly instructed to "delete"
- When uncertain, conservatively preserve existing information
- Respect existing structure and format, maintain consistency
- Add newline at end of files - avoid 'No newline at end of file' in diff output

## Fact-Checking

- Before relying on a technical claim, verify it by searching for official documentation or source code via WebSearch/WebFetch, and cite the URL
- If no primary source is found, mark the claim as **UNVERIFIED** and ask the user before proceeding
- Do not assume a tool's flag, feature, or policy exists — always check first
- Do not guess repository URLs — look them up
- When uncertain, say "I'm not certain about this — let me check" and investigate
