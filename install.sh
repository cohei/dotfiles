#!/bin/sh

set -eux

FLAKE=${1:-github:cohei/dotfiles}

# ncurses for tput
# inetutils for hostname
nix shell nixpkgs#git nixpkgs#ncurses nixpkgs#inetutils \
    --command nix run "${FLAKE}#home-manager" -- switch --flake "$FLAKE"
