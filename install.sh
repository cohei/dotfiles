FLAKE=${1:-github:cohei/dotfiles}
SYSTEM=$(nix eval --impure --raw --expr builtins.currentSystem)

home-manager switch --flake "${FLAKE}#${USER}:${SYSTEM}"
