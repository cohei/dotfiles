{ pkgs, ... }:

{
  home.packages = [
    (pkgs.callPackage ./derivation.nix { })
  ];
}
