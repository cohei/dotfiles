{
  packageOverrides = pkgs:
    with pkgs; {
      myPackages = buildEnv {
        name = "my-packages";
        paths = [
          bashCompletion
          bashInteractive_5
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
          haskellPackages.hpack
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
          nix
          nix-bash-completions
          pwgen
          sbt
          shellcheck
          stack
          terraform_0_12
          tmux
          tree
          watch
          wget
          xz # for ghcup
        ];
      };
    };
}
