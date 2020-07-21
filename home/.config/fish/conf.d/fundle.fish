if not functions --query fundle
    eval (curl --silent --fail --location https://git.io/fundle-install)
end

fundle plugin 'jethrokuan/z'
fundle plugin 'jorgebucaran/fish-bax'
fundle init

if test -e ~/.ghcup/env
    bax source ~/.ghcup/env
end

if type --quiet home-manager
    bax source "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
end
