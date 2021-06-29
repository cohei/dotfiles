{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  imports =
    let
      moduleDir = ./module;
      toModulePath = f: moduleDir + ("/" + f);
      modules = builtins.map toModulePath (builtins.attrNames (builtins.readDir moduleDir));
    in
      modules;

  home.packages =
    with pkgs;
    [
      abduco
      bat
      bottom
      coreutils
      du-dust
      dvtm
      exa
      fd
      ffmpeg
      fswatch
      fzf
      ghcid
      ghq
      google-cloud-sdk
      hadolint
      haskellPackages.cabal-fmt
      haskellPackages.hoogle
      haskellPackages.hpack
      hledger
      icdiff
      ipfs
      jl
      jq
      metals
      multitime
      ncdu
      nkf
      nodePackages.typescript-language-server
      parallel
      # pijul # 1.0.0-alpha.46 fails
      procs
      pstree
      pwgen
      rnix-lsp
      sbt
      shellcheck
      skktools
      solargraph
      stack
      terraform_0_13
      tokei
      watch
      wget
    ] ++ lib.optionals stdenv.isDarwin [ mas terminal-notifier ];

  home.file."." = {
    source = ./home;
    recursive = true;
  };

  home.language.base = "ja_JP.UTF-8";
  home.sessionPath = [ "$HOME/.local/bin" ];
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
          sha256 = "qt63SHfHWQnDBvaL+NOVgGA1Pt6tbWhV+1op0JoQNMM=";
        };
      }
      {
        name = "z";
        src = pkgs.fetchFromGitHub {
          owner = "jethrokuan";
          repo = "z";
          rev = "master";
          sha256 = "l5y4cFwO1Lqe/tFNO87F/zs8zA1JXLeYCIKuX6el5hc=";
        };
      }
    ];
    shellAbbrs = {
      aa = "arch -arm64";
      doco = "docker compose";
    };
    shellAliases = {
      ls = "exa --classify";
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
