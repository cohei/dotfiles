set -eux

has() {
    command -v "$1" >/dev/null 2>&1
}

install_home_manager() {
    nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
    nix-channel --update
    nix-shell '<home-manager>' -A install
}

repository=$HOME/src/github.com/cohei/dotfiles

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

if ! has home-manager; then
    install_home_manager
fi

[ -d "$repository" ] || download
link

home-manager switch
