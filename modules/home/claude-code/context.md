# User Memory

## Editing Style

When editing files, please follow these principles:

- **Always preserve existing correct information**
- Update = add/modify, not replace
- Only delete when explicitly instructed to "delete"
- When uncertain, conservatively preserve existing information
- Respect existing structure and format, maintain consistency
- Add newline at end of files - avoid 'No newline at end of file' in diff output

## Fact-Checking

- When investigating something, search the internet (official documentation, GitHub, WebSearch) first, then explore local files
- Before relying on a technical claim, verify it by searching for official documentation or source code via WebSearch/WebFetch, and cite the URL — especially tool flags, features, and repository URLs
- If no primary source is found, mark the claim as **UNVERIFIED** and ask the user before proceeding
- Distinguish speculation from verified conclusions

## Auto-Memory

- Save a feedback memory only when the preference recurs across instances, or when explicitly told to remember it; when unsure, ask rather than save
- Don't save a single, task-specific instruction to auto-memory as if it were a durable rule. A one-time correction is scoped to that task, not a standing preference

## VCS

- Run version control operations on the host side, not inside containers
- Detect the VCS before write operations: if a `.jj` directory exists, the repo uses Jujutsu (jj); otherwise git
- In jj repositories, use `jj` commands (`jj commit`, `jj new`, `jj describe`) instead of git for write operations
- Colocated repositories (both `.jj` and `.git`) allow read-only git commands (`git log`, `git diff`, `git grep`) — they are convenient and harmless

## Scratch Files

- Files you want to keep in the working tree but never commit (try-out scripts, experimental output, investigation notes and plans, downloaded references) go under the repo-root `my-scratch/` directory, which is globally gitignored and so never pollutes git status
- Create it on demand in whatever repo you are working in, and mention it the first time you do so there
- When a task ends, use judgment: delete the throwaway files you created, keep anything meant to persist (something the user may want to revisit), and never auto-delete a deliberate artifact
