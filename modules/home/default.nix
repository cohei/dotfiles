{ config, pkgs, inputs, ... }:

{
  home.stateVersion = "25.05";

  nixpkgs.overlays = [
    (_: _: { unfree = inputs.nixpkgs-unfree.legacyPackages.${pkgs.system}; })
    (_: _: { for-tup = inputs.nixpkgs-for-tup.legacyPackages.${pkgs.system}; })
  ];

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
      bash-language-server
      bottom
      coreutils
      duf
      dust
      dvtm
      fd
      ffmpeg
      fswatch
      google-cloud-sdk
      hadolint
      hledger
      hyperfine
      icdiff
      ipfs
      jq
      less
      metals
      multitime
      ncdu
      nixd
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
      for-tup.tup  # broken on Darwin in current nixpkgs
      typescript-language-server
      unused
      watch
      wget
      xdg-ninja
      xh
      yaml-language-server
      zstd
    ];

  home.language.base = "ja_JP.UTF-8";
  home.sessionPath = [ "$HOME/.local/bin" ];
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
      bell = {
        color = "#6c71c4"; # Solarized violet
        command = { program = "osascript"; args = [ "-e" "beep" ]; };
        duration = 500;
      };
      selection.save_to_clipboard = true;
      cursor = {
        style.blinking = "On";
        blink_timeout = 0;
      };
      keyboard.bindings = [
        # for Claude Code
        {
          key = "Enter";
          mods = "Shift";
          chars = "\\n";
        }
      ];
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
    extraOptions = [ "--classify" "--time-style=long-iso" ];
  };

  programs.fish = {
    enable = true;
    plugins = [
      {
        name = "nix.fish";
        src = pkgs.fetchFromGitHub {
          owner = "kidonng";
          repo = "nix.fish";
          rev = "master";
          sha256 = "sha256-GMV0GyORJ8Tt2S9wTCo2lkkLtetYv0rc19aA5KJbo48=";
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
    shellAbbrs.aa = "arch -arm64";
    shellInit = ''
      set fish_greeting
    '';
  };

  programs.fzf.enable = true;

  programs.mergiraf.enable = true;

  programs.pay-respects.enable = true;

  xdg.enable = true;
}
