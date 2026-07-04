{ config, lib, pkgs, hostName, perSystem, ... }:

{
  config = lib.mkIf pkgs.stdenv.isDarwin {
    home.homeDirectory = lib.mkForce "/Users/${config.home.username}";

    home.packages =
      let
        home-manager-news =
          perSystem.self.home-manager-news.override { inherit hostName; inherit (config.home) username; };
      in
        with pkgs; [
          home-manager-news
          mas
          net-news-wire
          perSystem.self.clean-links
          stats
          unfree.appcleaner
        ];

    home.file."iCloud Drive".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Library/Mobile Documents/com~apple~CloudDocs";

    programs.alacritty.settings.bell.command = { program = "osascript"; args = [ "-e" "beep" ]; };

    targets.darwin = {
      copyApps.enable = true;
      defaults."com.apple.dock".showhidden = true;
      linkApps.enable = false;
    };
  };
}
