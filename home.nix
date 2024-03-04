{ config, pkgs, ... }:

{
  home.stateVersion = "23.11";

  home.homeDirectory =
    let
      username = config.home.username;
    in
      if username == "root" then "/root" else "/home/${username}";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.packages =
    with pkgs;
    [
      abduco
      bottom
      coreutils
      du-dust
      duf
      dvtm
      fd
      ffmpeg
      fswatch
      fzf
      google-cloud-sdk
      hadolint
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
    ];

  home.file.".ghci".source = ./home/.ghci;

  home.language.base = "ja_JP.UTF-8";
  home.sessionPath = [ "$HOME/.local/bin" "$HOME/.docker/bin" ];
  home.sessionVariables = {
    GHCUP_USE_XDG_DIRS = "yes";
    LESS = "--LONG-PROMPT --RAW-CONTROL-CHARS --quit-if-one-screen --no-init";
  };

  programs.alacritty = {
    enable = true;
    settings = {
      font = {
        normal.family = "Cica";
        size = 14.5;
      };
      window = {
        padding = { x = 5; y = 5; };
        dimensions = { columns = 150; lines = 50; };
        option_as_alt = "Both";
      };
      cursor = {
        style.blinking = "On";
        blink_timeout = 0;
      };
    };
  };

  programs.bat = {
    enable = true;
    config = {
      theme = "base16-256";
    };
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.eza = {
    enable = true;
    enableAliases = true;
    extraOptions = [ "--classify" "--time-style=long-iso" ];
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
    '';
  };
}
