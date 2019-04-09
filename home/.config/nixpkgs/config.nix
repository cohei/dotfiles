{
  packageOverrides = pkgs:
    with pkgs; {
      myPackages = buildEnv {
        name = "my-packages";
        paths = [
          bashCompletion
          bashInteractive
          cacert # not to cause SSL error
          cloc
          coreutils
          direnv
          emacs
          fzf
          ghq
          git
          gitAndTools.diff-so-fancy
          gitAndTools.hub
          haskellPackages.ghcid
          haskellPackages.hlint
          haskellPackages.hoogle
          haskellPackages.hpack
          haskellPackages.stylish-haskell
          hledger
          ipfs
          jl
          jq
          nix
          nix-bash-completions
          pwgen
          sbt
          shellcheck
          stack
          terraform
          tmux
          tree
          watch
          wget
          xz # for ghcup
        ];
      };
    };
}
