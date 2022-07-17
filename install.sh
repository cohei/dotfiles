#!/bin/sh

set -eux

switch() {
    # ncurses for tput
    # inetutils for hostname
    nix shell nixpkgs#git nixpkgs#ncurses nixpkgs#inetutils \
        --command nix run "${1}#home-manager" -- switch --flake "$1"
}

switch "${1:-github:cohei/dotfiles}"
