{ pkgs, ... }:

{
  home.packages = [ pkgs.exa ];

  programs.fish.shellAliases = {
    ls = "exa --classify";
    la = "exa --classify --all";
    ll = "exa --classify --long";
    lla = "exa --classify --long --all";
    lt = "exa --classify --tree";
  };
}
