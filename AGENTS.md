# AGENTS.md

This file provides guidance to AI coding assistants when working with code in this repository.

## Repository Overview

This is a Nix-based dotfiles repository using the **numtide/blueprint** framework to standardize flake structure. It provides cross-platform user environment configuration via Home Manager with support for both Linux and macOS (Darwin).

- **Home Manager**: Manages user environment (dotfiles, packages, application settings) on both platforms
- **nix-darwin**: Declaratively manages macOS-specific system-level settings

## Repository Layout

Blueprint maps directories to flake outputs:

- `flake.nix` — `outputs = inputs: inputs.blueprint { inherit inputs; }`.
- `hosts/<host>/` → `nixosConfigurations.<host>` / `darwinConfigurations.<host>`; holds each host's `users/<user>.nix` (Home Manager) and, on macOS, `darwin-configuration.nix`.
- `modules/home/` → `homeModules.*`; `modules/darwin/` → `darwinModules.*`.
- `packages/` → `packages.*`.

## Hosts

`sandbox-mac` and `sandbox-container` exist so CI exercises the same configuration as the real machine, so divergence between hosts is kept minimal. A shared module that needs a host-specific value stays imported everywhere, with the value set in each `hosts/<host>/darwin-configuration.nix`; the import is not moved down to a single host.

## Module vs Package

- **`modules/home/<name>`** — code that *configures* the user environment (`home.packages`, `programs.*`, `xdg.configFile`, fish abbreviations, …).
- **`packages/<name>`** — standalone derivations (`writeShellApplication`, `mkDerivation`, Haskell `Main.hs` + `default.nix`, …), consumed from modules via `perSystem.self.<name>`.

Rule of thumb: if it *configures or installs* something for the user, it's a module; if it *builds* an artifact, it's a package.

Platform-specific settings are guarded with `lib.mkIf pkgs.stdenv.hostPlatform.isDarwin`.

## Claude user memory

Claude Code's global user memory (`~/.claude/CLAUDE.md`) is generated from `modules/home/claude-code/context.md` via `programs.claude-code.context`.

## Commands

- Apply the configuration: `nix run` (this checkout) or `nix run github:cohei/dotfiles` — both invoke `packages/activate.nix`, which runs `darwin-rebuild switch` (macOS) or `home-manager switch` (Linux).
- Or run those directly; use `build` instead of `switch` to test without activating (`home-manager build` / `darwin-rebuild build`).
- Isolated testing in a NixOS container: see [README.md](README.md).

## Change conventions

- Do not add `nativeBuildInputs`/`buildInputs` based on guesswork. Build with a minimal configuration first, and only add dependencies when the build actually fails.
- After running `nix build`, delete the `result` symlink (`rm result`) before committing.
- Commit subjects carry a `Component:` prefix, the component's own name (`Emacs: cabal-mode`, `Claude Code: run docker outside the sandbox`).
- When editing this file, write facts and conventions, not procedural how-to; omit standard tool behavior and directory listings (they go stale — state the pattern instead).
- Match each heading and subject to its scope: generic guidance as "AI agent", agent-specific facts (e.g. Claude's user memory) under that agent's name.
