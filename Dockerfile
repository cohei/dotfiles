FROM nixos/nix

# remove packages causing conflict
RUN nix-env --uninstall coreutils git man-db wget

RUN echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf
