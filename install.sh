#!/bin/bash

set -eux

repository=~/.dotfiles
dotfiles_dir="$repository/home"

download() {
    git clone https://github.com/cohei/dotfiles.git $repository
}

initialize() {
    # Git remembers only permissions about executability.
    # ghci reads .ghci when it does not allow to be written from other users.
    chmod 644 $dotfiles_dir/.ghci
}

link() {
    dist=~

    while read -r target_with_dot; do
	target=${target_with_dot#*/}

        dir=$(dirname "$target")
        [ -d "$dist/$dir" ] || mkdir -p "$dist/$dir"

        ln --symbolic --relative --backup "$dotfiles_dir/$target" "$dist/$target" || \
        ln --symbolic            --backup "$dotfiles_dir/$target" "$dist/$target" || \
        ln -s -f                          "$dotfiles_dir/$target" "$dist/$target"

    done < <(cd $dotfiles_dir && find . -type f)
}

[ -d $repository ] || download
initialize
link
