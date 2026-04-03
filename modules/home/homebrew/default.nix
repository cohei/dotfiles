{ lib, pkgs, ... }:

{
  config = lib.mkIf pkgs.stdenv.isDarwin {
    programs.fish.shellInit = ''
      if test -e /opt/homebrew/bin/brew
          eval (/opt/homebrew/bin/brew shellenv)
      end
    '';

    xdg.configFile."homebrew/Brewfile".source = ./Brewfile;
  };
}
