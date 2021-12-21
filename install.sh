#!/bin/sh

set -eux

switch() {
    # ncurses for tput
    # inetutils for hostname
    nix shell nixpkgs#git nixpkgs#ncurses nixpkgs#inetutils \
        --command nix run 'github:cohei/dotfiles#home-manager' -- switch --flake github:cohei/dotfiles
}

switch
