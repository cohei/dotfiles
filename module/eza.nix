{ pkgs, ... }:

{
  home.packages = [ pkgs.eza ];

  programs.fish.shellAliases = {
    ls = "eza --classify";
    la = "eza --classify --all";
    ll = "eza --classify --long";
    lla = "eza --classify --long --all";
    lt = "eza --classify --tree";
  };
}
