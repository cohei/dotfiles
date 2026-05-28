# AGENTS.md

This file provides guidance to AI coding assistants when working with code in this repository.

## Repository Overview

This is a Nix-based dotfiles repository using the **numtide/blueprint** framework to standardize flake structure. It provides cross-platform user environment configuration via Home Manager with support for both Linux and macOS (Darwin).

- **Home Manager**: Manages user environment (dotfiles, packages, application settings) on both platforms
- **nix-darwin**: Declaratively manages macOS-specific system-level settings

## Architecture

### Dependencies

Primary flake inputs: `nixpkgs`, `nixpkgs-unfree`, `home-manager`, `nix-darwin`, `blueprint`

Other inputs and their purpose: `nixpkgs-for-tup` pins a `tup` that works on Darwin; `anthropics-skills` and `serena` are for claude-code; `tinted-terminal` provides Alacritty themes.

### Structure

#### Directories

- **`flake.nix`**: Main Nix flake (Blueprint: `inputs.blueprint { inherit inputs; }`)
- **`hosts/`**: Blueprint standard convention: Machine configurations → `nixosConfigurations.*`/`darwinConfigurations.*`
  - Contains Home Manager configurations for each host/user
- **`modules/`**: Blueprint standard convention: NixOS/Darwin/Home Manager modules → `nixosModules.*`/`darwinModules.*`
  - Custom setup with Home Manager modules under `modules/home/`
- **`packages/`**: Blueprint standard convention: Project packages → `packages.*`
  - Custom packages and installation-related files

#### Module vs Package

- **`modules/home/<name>`** — code that *configures* the user environment (`home.packages`, `programs.*`, `xdg.configFile`, fish abbreviations, …).
- **`packages/<name>`** — standalone derivations (`writeShellApplication`, `mkDerivation`, Haskell `Main.hs` + `default.nix`, …), consumed from modules via `perSystem.self.<name>`.

Rule of thumb: if it *configures or installs* something for the user, it's a module; if it *builds* an artifact, it's a package.

Platform-specific settings are guarded with `lib.mkIf pkgs.stdenv.isDarwin`.

### Claude user memory

Claude Code's global user memory (`~/.claude/CLAUDE.md`) is generated from `modules/home/claude-code/context.md` via `programs.claude-code.context`.

## Repository Development

### Installation and Testing

- **Local**: `nix run`
- **From GitHub**: `nix run github:cohei/dotfiles`
- **Docker testing**: see [README.md](README.md)

### Configuration Application

After changes in `hosts/` or `modules/`, apply configuration with:
- **Linux**: `home-manager switch` / `home-manager build` (test only)
- **macOS**: `darwin-rebuild switch` / `darwin-rebuild build` (test only)

### Testing Environment

Use Docker (NixOS) container for isolated testing before pushing changes.

### Change conventions

- Do not add `nativeBuildInputs`/`buildInputs` based on guesswork. Build with a minimal configuration first, and only add dependencies when the build actually fails.
- After running `nix build`, delete the `result` symlink (`rm result`) before committing.
- When editing this file, write facts and conventions, not procedural how-to; omit standard tool behavior and directory listings (they go stale — state the pattern instead).
- Match each heading and subject to its scope: generic guidance as "AI agent", agent-specific facts (e.g. Claude's user memory) under that agent's name.
