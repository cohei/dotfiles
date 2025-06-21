{ pkgs, unfree, ... }:

{
  home.packages = [ unfree.claude-code pkgs.ripgrep ];
}
