# syntax=docker/dockerfile:1.7-labs
FROM nixos/nix

RUN echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf

# Set lower priority to avoid conflicts with dotfiles packages
RUN nix-env --set-flag priority 10 \
    coreutils-full \
    git-minimal \
    less \
    man-db \
    wget

WORKDIR /root/dotfiles
COPY --parents flake.lock flake.nix hosts modules packages ./
