{ pkgs, perSystem, ... }:

{
  programs.claude-code = {
    enable = true;
    package = pkgs.unfree.claude-code;
  };

  home.packages = [
    pkgs.ripgrep
    perSystem.serena.default
  ];
}
