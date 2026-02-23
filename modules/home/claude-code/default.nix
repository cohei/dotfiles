{ pkgs, perSystem, ... }:

{
  programs.claude-code = {
    enable = true;
    package = pkgs.unfree.claude-code;
    memory.source = ./memory.md;
  };

  home.packages = [
    pkgs.ripgrep
    perSystem.serena.default
  ];
}
