#!/bin/bash

set -eux

repository=~/.dotfiles

download() {
    git clone https://github.com/cohei/dotfiles.git $repository
}

initialize() {
    # git は実行可否以外のモードを保存しない
    # ユーザー以外が書き込み権限を持っていると読み込んでくれない
    chmod 644 $repository/.ghci
}

link() {
    dist=~

    targets=(
        .bash_aliases
        .bash_profile
        .bashrc
        .bundle/config
        .emacs.d/init.el
        .ghci
        .gitconfig
        .pryrc
        .rspec
        bin/diff-highlight
        bin/gem-uninstall-all
        bin/grepr
        .ssh/config
    )

    [ -d $dist/.bundle ]  || mkdir $dist/.bundle
    [ -d $dist/.emacs.d ] || mkdir $dist/.emacs.d
    [ -d $dist/bin ]      || mkdir $dist/bin
    [ -d $dist/.ssh ]     || mkdir $dist/.ssh

    for target in "${targets[@]}"; do
        ln --symbolic --relative --backup "$repository/$target" "$dist/$target" || \
        ln --symbolic            --backup "$repository/$target" "$dist/$target" || \
        ln -s -f                          "$repository/$target" "$dist/$target"
    done
}

[ -d $repository ] || download
initialize
link
