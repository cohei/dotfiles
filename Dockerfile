FROM nixos/nix

# lower priority of packages causing conflict
RUN nix-env --set-flag priority 0 coreutils-full git man-db wget

RUN echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf
