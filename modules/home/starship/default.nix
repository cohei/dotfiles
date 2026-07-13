{ pkgs, ... }:

{
  home.packages = [ pkgs.starship-jj ];

  programs.starship.enable = true;

  xdg.configFile = {
    "starship.toml".source = ./starship.toml;
    "starship-git.toml".source = ./starship-git.toml;
    "starship-jj/starship-jj.toml".source = ./starship-jj.toml;
  };
}
