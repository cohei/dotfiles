{ pkgs, perSystem, ... }:

{
  home.packages = [ perSystem.starship-jj.default ];

  programs.starship.enable = true;

  xdg.configFile = {
    # `applyPatches` requires a directory source
    "starship.toml".source = ./src/starship.toml;
    "starship-no-git.toml".source =
      "${pkgs.applyPatches { src = ./src; patches = [ ./starship-no-git.patch ]; }}/starship.toml";
    "starship-jj/starship-jj.toml".source = ./starship-jj.toml;
  };
}
