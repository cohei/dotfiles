#!/bin/bash

set -eux

repository=~/.dotfiles

download() {
    git clone https://github.com/cohei/dotfiles.git $repository
}

initialize() {
    # Git remembers only permissions about executability.
    # ghci reads .ghci when it does not allow to be written from other users.
    chmod 644 $repository/.ghci
}

link() {
    dist=~

    targets=(
        .Brewfile
        .bash_aliases
        .bash_profile
        .bashrc
        .bundle/config
        .config/git/ignore
        .emacs.d/init.el
        .ghci
        .gitconfig
        .pryrc
        .rspec
        .ssh/config
        bin/dotall
        bin/gem-uninstall-all
        bin/grepr
        bin/shell_expansion
    )

    for target in "${targets[@]}"; do
        dir=$(dirname "$target")
        [ -d "$dist/$dir" ] || mkdir -p "$dist/$dir"

        ln --symbolic --relative --backup "$repository/$target" "$dist/$target" || \
        ln --symbolic            --backup "$repository/$target" "$dist/$target" || \
        ln -s -f                          "$repository/$target" "$dist/$target"
    done
}

[ -d $repository ] || download
initialize
link
