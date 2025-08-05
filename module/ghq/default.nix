{ pkgs, ... }:

{
  home.packages = [ pkgs.ghq ];

  xdg.configFile."fish/functions/ghq-look.fish".source = ./ghq-look.fish;
}
