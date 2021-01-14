#!/bin/sh

set -eux

repository=$HOME/.local/share/dotfiles

download() {
    nix-shell --packages git --command 'git clone https://github.com/cohei/dotfiles.git '"$repository"
}

ensure_nix_flakes() {
    nix-env --install --attr nixpkgs.nixFlakes
}

switch() {
    nix --experimental-features 'nix-command flakes' shell nixpkgs#git \
        --command nix --experimental-features 'nix-command flakes' run "$repository#switch"
}

[ -d "$repository" ] || download

ensure_nix_flakes
switch
