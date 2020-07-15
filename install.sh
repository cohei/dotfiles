#!/bin/bash

set -eux

repository=${XDG_DATA_HOME:-$HOME/.local/share}/dotfiles

download() {
    git clone https://github.com/cohei/dotfiles.git "$repository"
}

link() {
    target=~/.config/nixpkgs/home.nix

    dir=$(dirname "$target")
    [ -d "$dir" ] || mkdir -p "$dir"

    ln --symbolic --relative --backup "$repository/home.nix" "$target" || \
    ln --symbolic            --backup "$repository/home.nix" "$target" || \
    ln -s -f                          "$repository/home.nix" "$target"
}

[ -d "$repository" ] || download
link
