{ config, lib, pkgs, hostName, ... }:

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
          (callPackage ./clean-links.nix {})
          home-manager-news
          stats
          unfree.appcleaner
        ];

    home.file."iCloud Drive".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Library/Mobile Documents/com~apple~CloudDocs";

    programs.fish.shellInit = ''
      if test -e /opt/homebrew/bin/brew
          eval (/opt/homebrew/bin/brew shellenv)
      end
    '';

    targets.darwin = {
      copyApps.enable = true;
      defaults."com.apple.dock".showhidden = true;
      linkApps.enable = false;
    };

    xdg.configFile."homebrew/Brewfile".source = ./Brewfile;
  };
}
