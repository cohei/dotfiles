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
  home.stateVersion = "20.03";

  home.packages =
    with pkgs;
    let
      enhancd = import ./enhancd.nix { inherit stdenv fetchFromGitHub runtimeShell; };
    in
      [
        bashCompletion
        bashInteractive_5
        bat
        cloc
        cmake
        coreutils
        direnv
        dust
        emacs
        enhancd
        exa
        fd
        ffmpeg
        fish
        fswatch
        fzf
        ghq
        git
        gitAndTools.diff-so-fancy
        gitAndTools.gh
        gitAndTools.hub
        google-cloud-sdk
        # haskellPackages.cabal-fmt # compile error
        haskellPackages.ghcid
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
        nix-bash-completions
        nkf
        parallel
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
        tree
        watch
        wget
        ytop
      ];
}
