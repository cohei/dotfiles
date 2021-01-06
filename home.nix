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
      bottom
      cmake # Emacs vterm package
      cmigemo
      coreutils
      du-dust
      emacs
      exa
      fd
      ffmpeg
      fswatch
      fzf
      ghcid
      ghq
      git
      gitAndTools.delta
      gitAndTools.gh
      google-cloud-sdk
      hadolint
      haskellPackages.cabal-fmt
      haskellPackages.hoogle
      haskellPackages.hpack
      hledger
      hlint
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
      stylish-haskell
      terraform_0_13
      tmux
      tokei
      watch
      wget
    ] ++ lib.optionals stdenv.isDarwin [ terminal-notifier ];

  home.file."." = {
    source = ./home;
    recursive = true;
  };

  home.homeDirectory = "/hoge";
  home.username = "fuga";

  home.language.base = "ja_JP.UTF-8";
  home.sessionPath = [ "$HOME/.local/bin" ];
  home.sessionVariables = {
    # for
    #   - git commiting
    #   - less v
    EDITOR =
      let
        options =
          if pkgs.stdenv.isDarwin
          then "--alternate-editor='open -a emacs'"
          else "--alternate-editor='' --create-frame";
      in "emacsclient ${options}";
    GHCUP_USE_XDG_DIRS = "yes";
    LESS = "--LONG-PROMPT --RAW-CONTROL-CHARS --quit-if-one-screen --no-init";
  };

  programs.bash = {
    enable = true;
    initExtra = ''
      if command -v fish > /dev/null && [ -z "$BASH_EXECUTION_STRING" ]; then
        exec fish
      fi
    '';
    profileExtra = ''
      if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then
        . ~/.nix-profile/etc/profile.d/nix.sh
      fi
    '';
  };

  programs.direnv = {
    enable = true;
    enableNixDirenvIntegration = true;
  };

  programs.fish = {
    enable = true;
    shellInit = ''
      set fish_greeting

      if test -e ~/.nix-profile/share/chruby/chruby.fish
          source ~/.nix-profile/share/chruby/chruby.fish
          source ~/.nix-profile/share/chruby/auto.fish
      end

      if type --quiet gh
          eval (gh completion --shell fish)
      end
    '';
  };

  programs.starship = {
    enable = true;
  };
}
