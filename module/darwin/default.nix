{ config, lib, pkgs, unfree, isDarwin, mac-app-util, ... }:

lib.attrsets.optionalAttrs isDarwin {
  imports = [ mac-app-util.homeManagerModules.default ];

  config = {
    home.homeDirectory = lib.mkForce "/Users/${config.home.username}";

    home.packages =
      let
        touch-sudo = pkgs.writeShellApplication {
          name = "touch-sudo";
          runtimeInputs = [ pkgs.gnused ];
          text = ''
            sed --in-place=.bak '2i auth       sufficient     pam_tid.so' /etc/pam.d/sudo
          '';
        };
      in
        with pkgs; [
          unfree.appcleaner
          mas
          (callPackage ./clean-links.nix {})
          touch-sudo
        ];

    home.file."iCloud Drive".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Library/Mobile Documents/com~apple~CloudDocs";

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

    xdg.configFile."homebrew/Brewfile".source = ./Brewfile;
  };
}
