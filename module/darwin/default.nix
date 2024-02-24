{ config, lib, pkgs, mkalias, ... }:

lib.mkIf pkgs.stdenv.isDarwin {
  home.homeDirectory = lib.mkForce "/Users/${config.home.username}";

  home.packages =
    with pkgs; [
      unfree.appcleaner
      mas
      terminal-notifier
      (callPackage ./clean-links.nix {})
    ];

  home.file.".Brewfile".source = ./.Brewfile;

  home.activation.aliasApplications =
    let
      apps = pkgs.buildEnv {
        name = "home-manager-applications";
        paths = config.home.packages;
        pathsToLink = "/Applications";
      };
    in
      lib.hm.dag.entryAfter ["writeBoundary"] ''
        aliasDirectory="${config.home.homeDirectory}/Applications/Home Manager App Aliases"

        $DRY_RUN_CMD mkdir -p "$aliasDirectory"
        $DRY_RUN_CMD rm -f "$aliasDirectory"/*

        for app in ${apps}/Applications/*.app; do
          $DRY_RUN_CMD ${mkalias} --read-link "$app" "$aliasDirectory/$(basename "$app")"
        done
      '';

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
}
