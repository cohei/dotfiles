#!/bin/sh

set -eux

ensure_nix_flakes() {
    nix-channel --update
    nix-env --install --attr nixpkgs.nixFlakes
}

switch() {
    nix shell nixpkgs#git --command nix run 'github:cohei/dotfiles#switch'
}

ensure_nix_flakes
switch
