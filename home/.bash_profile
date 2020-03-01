export LANG=ja_JP.UTF-8

export PATH=~/.local/bin:$PATH

# added by Nix installer
if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then
    . ~/.nix-profile/etc/profile.d/nix.sh
fi

if [ -d ~/.ghcup ]; then
    . ~/.ghcup/env
fi

# shellcheck source=.bashrc
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi
