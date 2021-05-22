FROM nixos/nix

RUN echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf
