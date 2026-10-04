---
name: nixpkgs-contribution
description: Conventions for working in a nixpkgs checkout. Invoke when editing a nixpkgs clone, before writing any commit message in one, and before opening a nixpkgs pull request.
---

# nixpkgs contribution

## Automation/AI Policy

Use of AI here is governed by `CONTRIBUTING.md` § "Automation/AI policy", and a deliberate violation is considered to break the Code of Conduct. Read that section for anything this skill doesn't cover.

Disclose with `Assisted-by: Claude Code (<model>)`, naming the model of the session that wrote it (`Claude Opus 5`, `Claude Sonnet 5`, …), not a fixed string, which is why past commits differ. Spell the display name as the default `Co-Authored-By` trailer does (user's decision).

- Commit: § "Transparency" requires this as a trailer and says `Co-authored-by:` does not satisfy it, so it replaces the `Co-Authored-By:` line the general instructions ask for
- Pull request body, review, comment: these need disclosure separate from the commits, in any adequate form. End them with the same line, in place of the footer the harness appends

## New Packages

Every new package under `pkgs/by-name/` must set both attrs:

```nix
__structuredAttrs = true;
strictDeps = true;
```

Otherwise CI's `Lint / nixpkgs-vet` fails with NPV-166 (`__structuredAttrs`) and NPV-164 (`strictDeps`). These are ratchet rules: older packages are grandfathered, but don't regress one (NPV-167 / NPV-165).

If turning the attrs on breaks the build, fix the underlying build problem. Never drop the attrs to make the build pass.

## Building

`CONTRIBUTING.md` § "Tested using sandboxing" asks for builds with the sandbox on, but the local Nix runs with `sandbox = false`. Build with `--option sandbox relaxed`.

## Pull Request

Open the pull request as a draft. Marking it ready for review requires nixpkgs-review to pass.

### Description

"Built on platform" lists only the platforms built locally as in [Building](#building). nixpkgs-review-gha results go under "Ran `nixpkgs-review`".

### Review with nixpkgs-review-gha

Review the pull request by running the `review.yml` workflow of the fork [cohei/nixpkgs-review-gha](https://github.com/cohei/nixpkgs-review-gha).

- Sync with upstream first
- Set `x86_64-darwin` to `no`. The default is `yes_sandbox_relaxed`, but nixpkgs has dropped x86_64-darwin: `lib/systems/doubles.nix` lists only `aarch64-darwin` for Darwin
- The result comment is posted even when the `report` job failed. Judge the builds from the run's `review (<system>)` jobs
- `on-success=mark_as_ready` does not work. With no `secrets.GH_TOKEN` in the fork, results are posted through nrgha-api, which cannot mark a pull request ready (`API error: cannot mark PRs as ready for review yet`, failing the `report` job)
