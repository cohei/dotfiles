set -eux

repository=~/.dotfiles

download() {
    git clone git@github.com:cohei/dotfiles.git $repository
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
        .bundle
        .emacs.d/init.el
        .ghci
        .gitconfig
        .pryrc
        .rspec
        bin/diff-highlight
        bin/grepr
        bin/psgrep
    )

    # $dist/.bundle のリンクがあるまま ln すると $dist/.bundle/.bundle というリンクができてしまう
    [ -L $dist/.bundle ] && rm $dist/.bundle

    [ -d $dist/.emacs.d ] || mkdir $dist/.emacs.d
    [ -d $dist/bin ]      || mkdir $dist/bin

    for target in ${targets[@]}; do
        ln -s -b $repository/$target $dist/$target
    done
}

[ -d $repository ] || download
initialize
link
