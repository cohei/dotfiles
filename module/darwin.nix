{ config, lib, pkgs, ... }:

lib.mkIf pkgs.stdenv.isDarwin {
  home.homeDirectory = lib.mkForce "/Users/${config.home.username}";

  home.packages =
    with pkgs; [
      mas
      terminal-notifier
    ];

  home.file."iCloud Drive".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Library/Mobile Documents/com~apple~CloudDocs";
}
