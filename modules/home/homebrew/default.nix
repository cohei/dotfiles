{ lib, pkgs, ... }:

{
  config = lib.mkIf pkgs.stdenv.isDarwin {
    xdg.configFile."homebrew/Brewfile".source = ./Brewfile;
  };
}
