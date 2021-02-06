#!/bin/sh

set -eux

ensure_nix_flakes() {
    nix-env --install --attr nixpkgs.nixFlakes
}

switch() {
    nix --experimental-features 'nix-command flakes' shell nixpkgs#git \
        --command nix --experimental-features 'nix-command flakes' run 'github:cohei/dotfiles#switch'
}

ensure_nix_flakes
switch
