{ config, lib, pkgs, ... }:

lib.mkIf pkgs.stdenv.isDarwin {
  home.homeDirectory = lib.mkForce "/Users/${config.home.username}";

  home.packages =
    with pkgs; [
      unfree.appcleaner
      mas
      terminal-notifier
      (callPackage ./clean-links.nix {})
    ];

  home.file = {
    ".Brewfile".source = ./.Brewfile;
    "iCloud Drive".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Library/Mobile Documents/com~apple~CloudDocs";
  };

  programs.fish.shellInit = ''
    if test -e /opt/homebrew/bin/brew
        eval (/opt/homebrew/bin/brew shellenv)
    end
  '';

  targets.darwin.defaults = {
    "com.apple.dock" = {
      showhidden = true;
    };
  };
}
