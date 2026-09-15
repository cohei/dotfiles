# User Memory

## Working with the User

- State moves between turns, by the user's hand or by another session: files, the commit graph, what is deployed. Re-read whatever you are about to act on rather than trusting what you saw earlier, and take it as is, including what is no longer there
- When the only thing left in doubt is a fact about the user's environment, asking is an option alongside checking it yourself. The user watches closely and often answers faster than a build or a test run can

## What You Produce

### Prose

- Dashes in prose read as affected; write plainly instead
- Prefer citing sources with named footnotes (`[^some-name]`) over inline URLs, especially in a longer document
- Use a real heading at the right level instead of bold text as a pseudo-heading
- A document states the current situation and is updated when a fact stops being true, so leave out "last updated" lines and "as of" or "re-verified" notes; dates belong only to external historical facts. Evidence pointers (what was grepped, a log file path) are content, keep them

### Code

- Order sibling things (if/else branches, attrset entries, enum cases) with the special or undesirable case first and the normal case after
- Don't abbreviate names you invent; name the role, `library` and `app` rather than `a` and `b`
- Reserve comments for non-obvious rationale. What the code, or the fact that it builds, already proves needs no comment; convey it through structure instead
- YAGNI applies to defensive lines too: add a guard only after showing that the failure actually happens, otherwise leave it out and offer it as a choice
- Add newline at end of files so diffs don't show 'No newline at end of file'

## Verifying and Arguing

### Facts

- Verify a technical claim about an external tool or library (a flag, a feature, a repository URL) against official documentation or source code via WebSearch/WebFetch before trusting what local files say, and cite the URL
- If no primary source is found, say so (mark the claim **UNVERIFIED**) and ask before proceeding; keep speculation visibly apart from verified conclusions in any report
- When research needs several files of an external repository, or a grep over it, clone it (shallow is fine) into `my-scratch/`: a checkout can be grepped and read whole, while WebFetch returns one page at a time, summarized

### Claims and Recommendations

- Before saying that something is done or already in the desired state, open the file and quote the lines; a clean worktree only shows the work was saved, and a local commit only shows it was recorded, not that either meets the spec
- Before calling one approach cleaner than another, state the concrete cost of each (bytes, lines, runtime, maintenance); if unmeasured, say so and present them as roughly equivalent
- Argue from properties of the result (clarity, locality, correctness, size), never from how small the diff is; a chain of minimal diffs does not add up to a good result

## Tooling

### VCS

- Run version control operations on the host side, not inside containers

### Scratch Files

- Files you want to keep in the working tree but never commit (try-out scripts, experimental output, investigation notes and plans, downloaded references) go under the repo-root `my-scratch/` directory, which is globally gitignored and so never pollutes git status
- Create it on demand in whatever repo you are working in, and mention it the first time you do so there
- When a task ends, use judgment: delete the throwaway files you created, keep anything meant to persist (something the user may want to revisit), and never auto-delete a deliberate artifact

### Environment

- `sudo` on macOS authenticates with Touch ID, so an AI agent can run a `sudo` command itself rather than handing it back to the user

### Claude Code

- Save a feedback memory only when the preference recurs across instances, or when explicitly told to remember it; a one-time correction is scoped to that task, not a standing preference. When unsure, ask rather than save
- Text written before an AskUserQuestion call in the same turn is not displayed[^ask-user-question-issue]; end the turn with the explanation, then ask in the next

[^ask-user-question-issue]: https://github.com/anthropics/claude-code/issues/75182
