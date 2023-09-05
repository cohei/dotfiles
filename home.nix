{ config, pkgs, ... }:

let
  isDarwin = pkgs.stdenv.isDarwin;
in
{
  home.stateVersion = "23.05";

  home.homeDirectory =
    let
      username = config.home.username;
    in
      if isDarwin
      then "/Users/${username}"
      else if username == "root" then "/root" else "/home/${username}";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.packages =
    with pkgs;
    [
      abduco
      bat
      bottom
      coreutils
      du-dust
      duf
      dvtm
      fd
      ffmpeg
      fswatch
      fzf
      ghq
      google-cloud-sdk
      # hadolint # broken
      haskellPackages.cabal-fmt
      haskellPackages.hoogle
      hledger
      hpack
      httpie
      hyperfine
      icdiff
      ipfs
      jq
      metals
      multitime
      ncdu
      nil
      nodePackages.bash-language-server
      nodePackages.typescript-language-server
      parallel
      pijul
      procs
      pstree
      pwgen
      sbt
      shellcheck
      skktools
      tldr
      tokei
      # tup # broken
      unused
      watch
      wget
      yaml-language-server
    ] ++ lib.optionals isDarwin [ mas terminal-notifier ];

  home.file =
    let
      iCloudDrivePath = "${config.home.homeDirectory}/Library/Mobile Documents/com~apple~CloudDocs";
      iCloudDriveLink =
        pkgs.lib.attrsets.optionalAttrs isDarwin {
          "iCloud Drive".source = config.lib.file.mkOutOfStoreSymlink iCloudDrivePath;
        };
    in {
      ".Brewfile".source = ./home/.Brewfile;
      ".config" = {
        source = ./home/.config;
        recursive = true;
      };
      ".ghci".source = ./home/.ghci;
    } // iCloudDriveLink;

  home.language.base = "ja_JP.UTF-8";
  home.sessionPath = [ "$HOME/.local/bin" "$HOME/.docker/bin" ];
  home.sessionVariables = {
    GHCUP_USE_XDG_DIRS = "yes";
    LESS = "--LONG-PROMPT --RAW-CONTROL-CHARS --quit-if-one-screen --no-init";
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.fish = {
    enable = true;
    plugins = [
      {
        name = "nix-env.fish";
        src = pkgs.fetchFromGitHub {
          owner = "lilyball";
          repo = "nix-env.fish";
          rev = "master";
          sha256 = "RG/0rfhgq6aEKNZ0XwIqOaZ6K5S4+/Y5EEMnIdtfPhk=";
        };
      }
      {
        name = "z";
        src = pkgs.fetchFromGitHub {
          owner = "jethrokuan";
          repo = "z";
          rev = "master";
          sha256 = "sha256-+FUBM7CodtZrYKqU542fQD+ZDGrd2438trKM0tIESs0=";
        };
      }
    ];
    shellAbbrs = {
      aa = "arch -arm64";
      doco = "docker compose";
    };
    shellInit = ''
      set fish_greeting

      if test -e /opt/homebrew/bin/brew
          eval (/opt/homebrew/bin/brew shellenv)
      end
    '';
  };

  programs.starship = {
    enable = true;
  };
}
