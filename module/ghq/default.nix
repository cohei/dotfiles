{ pkgs, ... }:

{
  home.packages = [ pkgs.ghq ];

  home.file.".config/fish/functions/ghq-look.fish".source = ./ghq-look.fish;
}
