{ config, lib, pkgs, hostName, perSystem, ... }:

{
  config = lib.mkIf pkgs.stdenv.isDarwin {
    home.homeDirectory = lib.mkForce "/Users/${config.home.username}";

    home.packages =
      let
        home-manager-news = pkgs.writeShellApplication {
          name = "home-manager-news";
          runtimeInputs = [ pkgs.home-manager ];
          text = ''
            home-manager news --flake .#${config.home.username}@${hostName}
          '';
        };
      in
        with pkgs; [
          perSystem.self.clean-links
          home-manager-news
          stats
          unfree.appcleaner
        ];

    home.file."iCloud Drive".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Library/Mobile Documents/com~apple~CloudDocs";

    targets.darwin = {
      copyApps.enable = true;
      defaults."com.apple.dock".showhidden = true;
      linkApps.enable = false;
    };
  };
}
