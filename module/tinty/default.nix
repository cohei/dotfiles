{ pkgs, ... }:

{
  home.packages = [
    (pkgs.callPackage ./derivation.nix {})
  ];

  programs.fish.interactiveShellInit = ''
    tinty apply base16-solarized-dark
  '';
}
