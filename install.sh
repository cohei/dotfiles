#!/bin/sh

set -eux

ensure_nix_flakes() {
    nix-channel --update
    nix-env --install --attr nixpkgs.nixFlakes
}

switch() {
    # ncurses for tput
    # inetutils for hostname
    nix shell nixpkgs#git nixpkgs#ncurses nixpkgs#inetutils \
        --command nix run 'github:cohei/dotfiles#home-manager' -- switch --flake github:cohei/dotfiles
}

ensure_nix_flakes
switch
