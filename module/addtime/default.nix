{ pkgs, ... }:

{
  nixpkgs.overlays = [
    (self: super: {
      addtime = super.callPackage ./derivation.nix {};
    })
  ];

  home.packages = [
    pkgs.addtime
  ];
}
