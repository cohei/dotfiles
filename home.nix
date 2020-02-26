{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.09";

  home.packages =
    with pkgs;
    [
      bashInteractive_5
      bat
      cmake
      cmigemo
      coreutils
      direnv
      dust
      emacs
      exa
      fd
      ffmpeg
      fish
      fswatch
      fzf
      ghcid
      ghq
      git
      gitAndTools.delta
      gitAndTools.gh
      google-cloud-sdk
      # haskellPackages.cabal-fmt # compile error
      # haskellPackages.hadolint # broken
      haskellPackages.hlint
      haskellPackages.hoogle
      haskellPackages.hpack
      haskellPackages.stylish-haskell
      # haskellPackages.unused # broken
      hledger
      icdiff
      ipfs
      jl
      jq
      libvterm-neovim # Emacs vterm package
      nkf
      parallel
      pijul
      procs
      pstree
      pwgen
      sbt
      shellcheck
      skktools
      solargraph
      stack
      starship
      terminal-notifier
      terraform_0_12
      tmux
      tokei
      tree
      watch
      wget
      ytop
    ];

  home.file = {
    ".bash_profile".source = ./home/.bash_profile;
    ".bashrc".source = ./home/.bashrc;
    ".Brewfile".source = ./home/.Brewfile;
    ".bundle/config".source = ./home/.bundle/config;
    ".config/fish" = {
      source = ./home/.config/fish;
      recursive = true;
    };
    ".config/gh/config.yml".source = ./home/.config/gh/config.yml;
    ".config/git" = {
      source = ./home/.config/git;
      recursive = true;
    };
    ".config/starship.toml".source = ./home/.config/starship.toml;
    ".emacs.d/init.el".source = ./home/.emacs.d/init.el;
    ".ghci".source = ./home/.ghci;
    ".local/bin" = {
      source = ./home/.local/bin;
      recursive = true;
    };
    ".ssh/config".source = ./home/.ssh/config;
  };

  home.homeDirectory = "";
  home.username = "";
}
