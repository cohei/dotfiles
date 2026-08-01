# syntax=docker/dockerfile:1.7-labs
FROM nixos/nix

# Set lower priority to avoid conflicts with dotfiles packages
RUN nix-env --set-flag priority 10 \
    coreutils-full \
    git-minimal \
    less \
    man-db \
    wget

WORKDIR /root/dotfiles
COPY --parents flake.lock flake.nix hosts modules nix-preamble.conf packages ./
RUN echo "include $PWD/nix-preamble.conf" >> /etc/nix/nix.conf
