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

## VCS

- Run version control operations on the host side, not inside containers
- Detect the VCS before write operations: if a `.jj` directory exists, the repo uses Jujutsu (jj); otherwise git
- In jj repositories, use `jj` commands (`jj commit`, `jj new`, `jj describe`) instead of git for write operations
- Colocated repositories (both `.jj` and `.git`) allow read-only git commands (`git log`, `git diff`, `git grep`) — they are convenient and harmless
