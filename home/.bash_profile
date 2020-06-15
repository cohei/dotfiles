# added by Nix installer
if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then
    . ~/.nix-profile/etc/profile.d/nix.sh
fi

# shellcheck source=.bashrc
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi
