FROM nixos/nix

RUN echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf

# Set lower priority to avoid conflicts with dotfiles packages
RUN nix-env --set-flag priority 10 \
    coreutils-full \
    git-minimal \
    less \
    man-db \
    wget
