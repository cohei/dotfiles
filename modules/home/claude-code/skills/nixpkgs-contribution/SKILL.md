---
name: nixpkgs-contribution
description: Conventions for working in a nixpkgs checkout. Invoke when editing a nixpkgs clone, before writing any commit message in one, and before opening a nixpkgs PR.
---

# nixpkgs contribution

## Automation/AI Policy

Use of AI here is governed by `CONTRIBUTING.md` § "Automation/AI policy", and a deliberate violation is considered to break the Code of Conduct. Read that section for anything the points below don't cover.

- Disclose a commit with `Assisted-by: Claude Code (<model>)`, naming the model of the session that wrote it (`Claude Opus 5`, `Claude Sonnet 5`, …) — not a fixed string, which is why past commits differ. Spell the display name as the default `Co-Authored-By` trailer does (user's decision)
- § "Transparency" requires the `Assisted-by:` trailer for a commit, in place of the `Co-Authored-By:` line the general instructions ask for
- PR summaries and review comments are covered too, and need disclosure separate from the commits. The footer the harness appends covers a PR body; an LLM-drafted comment or review needs its own line

## New Packages

Every new package under `pkgs/by-name/` must set both attrs:

```nix
__structuredAttrs = true;
strictDeps = true;
```

Otherwise CI's `Lint / nixpkgs-vet` fails with NPV-166 (`__structuredAttrs`) and NPV-164 (`strictDeps`). These are ratchet rules: older packages are grandfathered, but don't regress one (NPV-167 / NPV-165).

If turning the attrs on breaks the build, fix the underlying build problem. Never drop the attrs to make the build pass.

## Review Before a PR

Review a PR through the fork [cohei/nixpkgs-review-gha](https://github.com/cohei/nixpkgs-review-gha) (upstream is Defelo/nixpkgs-review-gha):

```
gh repo sync cohei/nixpkgs-review-gha
gh workflow run review.yml --repo cohei/nixpkgs-review-gha -f pr=<PR number> -f x86_64-darwin=no
```

- `x86_64-darwin=no` is required. The default is `yes_sandbox_relaxed`, but nixpkgs has dropped x86_64-darwin: `lib/systems/doubles.nix` lists only `aarch64-darwin` for Darwin
- `on-success=mark_as_ready` does not work. With no `secrets.GH_TOKEN` in the fork, results are posted through nrgha-api, which cannot mark a PR ready (`API error: cannot mark PRs as ready for review yet`, failing the `report` job). Un-draft manually with `gh pr ready <PR number>`
- The result comment is posted even when the `report` job failed. Judge the builds from the `review (<system>)` jobs in `gh run view <run-id> --repo cohei/nixpkgs-review-gha --json jobs`
