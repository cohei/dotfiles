{ pkgs, perSystem, ... }:

{
  home.packages = [
    pkgs.unfree.claude-code
    pkgs.ripgrep
    perSystem.serena.default
  ];
}
