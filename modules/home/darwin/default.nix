{ config, lib, pkgs, inputs, ... }:

{
  imports = [ inputs.mac-app-util.homeManagerModules.default ];

  config = lib.mkIf pkgs.stdenv.isDarwin {
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
          (callPackage ./clean-links.nix {})
          mas
          touch-sudo
          unfree.appcleaner
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
