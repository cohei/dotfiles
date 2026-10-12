---
name: nixpkgs-contribution
description: Conventions for contributing to nixpkgs. Invoke when editing a nixpkgs clone, before writing any commit message in one, and before opening, reviewing or commenting on a nixpkgs pull request.
---

# nixpkgs contribution

## Automation/AI Policy

Use of AI here is governed by `CONTRIBUTING.md` § "Automation/AI policy". Read that section for anything this skill doesn't cover.

Disclose with `Assisted-by: Claude Code (<model>)`, spelling this session's model as the default `Co-Authored-By` trailer does.

- Commit: as a trailer, replacing the `Co-Authored-By:` line the general instructions ask for
- Pull request body, review, comment: end them with the same line, in place of the footer the harness appends

## New Packages

Every new package under `pkgs/by-name/` must set both attrs:

```nix
__structuredAttrs = true;
strictDeps = true;
```

Otherwise CI's `Lint / nixpkgs-vet` fails with NPV-166 (`__structuredAttrs`) and NPV-164 (`strictDeps`). These are ratchet rules: older packages are grandfathered, but don't regress one (NPV-167 / NPV-165).

Never drop the attrs to make the build pass.

## Workflow

Go through the steps without asking, including pushing and force pushing to the fork (`origin`), opening the draft pull request, running the review workflow, and editing the pull request body. Stop for the user's approval only at a step's checkpoint. A step's "Template items" line names the checkboxes of the pull request template that its work lets you check.

### Edit

Template items: "Fits CONTRIBUTING.md, pkgs/README.md, maintainers/README.md and other READMEs", "Nixpkgs Release Notes", "NixOS Release Notes".

### Build and Test Locally

Template items: "Built on platform"; under "Tested, as applicable", every item except "Ran `nixpkgs-review` on this PR".

The local Nix runs with `sandbox = false`, so build with `--option sandbox relaxed`.

"Built on platform" lists only the platforms built here.

### Commit

Checkpoint: writing or changing a message. Squashing a fix into an existing commit while keeping its message is not one.

### Open the Pull Request

Template item: "Follows the automation/AI policy".

Open the pull request as a draft. Fill the template from the checkout, checking only the items the steps above produced evidence for.

### Run nixpkgs-review

Template item: "Ran `nixpkgs-review` on this PR" under "Tested, as applicable".

Review the pull request by running the `review.yml` workflow of the fork `cohei/nixpkgs-review-gha`.

- Sync the fork with its parent first
- Set `x86_64-darwin` to `no`; nixpkgs has dropped that platform

### Mark Ready for Review

The pull request is ready when every `review (<system>)` job and every upstream CI check succeed. Then check "Ran `nixpkgs-review`" in the body with a link to the result comment.

Checkpoint: marking the pull request ready.
