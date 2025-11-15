# AGENTS.md

This file provides guidance to AI coding assistants when working with code in this repository.

## Repository Overview

This is a Nix-based dotfiles repository using the **numtide/blueprint** framework to standardize flake structure. It provides cross-platform user environment configuration via Home Manager with support for both Linux and macOS (Darwin).

- **Home Manager**: Manages user environment (dotfiles, packages, application settings) on both platforms
- **nix-darwin**: Declaratively manages macOS-specific system-level settings

## Architecture

### Dependencies

Primary flake inputs: `nixpkgs`, `nixpkgs-unfree`, `home-manager`, `nix-darwin`, `blueprint`

Exceptional inputs (e.g., `nixpkgs-for-tup` for Darwin-specific fixes) are placed at the end of `flake.nix`.

### Structure

#### Directories

- **`flake.nix`**: Main Nix flake (Blueprint: `inputs.blueprint { inherit inputs; }`)
- **`hosts/`**: Blueprint standard convention: Machine configurations → `nixosConfigurations.*`/`darwinConfigurations.*`
  - Contains Home Manager configurations for each host/user
- **`modules/`**: Blueprint standard convention: NixOS/Darwin/Home Manager modules → `nixosModules.*`/`darwinModules.*`
  - Custom setup with Home Manager modules under `modules/home/`
- **`packages/`**: Blueprint standard convention: Project packages → `packages.*`
  - Custom packages and installation-related files

#### Modules (`modules/home/`)

**Development Tools**: `emacs/`, `git/`, `haskell/`, `claude-code.nix`
**CLI Enhancement**: `starship/`, `jujutsu.nix`, `ghq/`
**Custom Utilities**: `addtime/`, `level-payment/`, `shell-expansion.nix`, `dotall.nix`, `ccusage.nix`
**Platform-Specific**: `darwin/` (macOS-only), `docker.nix`, `tinty.nix`

Platform-specific settings use `isDarwin` flag (lib.attrsets.optionalAttrs).

## Repository Development

### Installation and Testing

- **Local**: `nix run . -- .`
- **From GitHub**: `nix run github:cohei/dotfiles`
- **Docker testing**: `docker compose run development [command]`

### Configuration Application

After changes in `hosts/` or `modules/`, apply configuration with:
- **Linux**: `home-manager switch` / `home-manager build` (test only)
- **macOS**: `darwin-rebuild switch` / `darwin-rebuild build` (test only)

### Testing Environment

Use Docker (NixOS) container for isolated testing before pushing changes.
