#!/bin/sh

set -eux

FLAKE=${1:-github:cohei/dotfiles}
SYSTEM=$(nix eval --impure --raw --expr builtins.currentSystem)

nix shell nixpkgs#git \
    --command nix run "${FLAKE}#home-manager" -- switch --flake "${FLAKE}#${USER}:${SYSTEM}"
