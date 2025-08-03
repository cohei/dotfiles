{ pkgs, ... }:

{
  home.packages = [ pkgs.unfree.claude-code pkgs.ripgrep ];
}
