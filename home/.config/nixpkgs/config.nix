{
  packageOverrides = pkgs:
    with pkgs;
    let
      enhancd = import ./enhancd.nix { inherit stdenv fetchFromGitHub runtimeShell; };
    in {
      myPackages = buildEnv {
        name = "my-packages";
        paths = [
          bashCompletion
          bashInteractive_5
          cacert # not to cause SSL error
          cloc
          cmake
          coreutils
          direnv
          emacs
          enhancd
          fzf
          ghq
          git
          gitAndTools.diff-so-fancy
          gitAndTools.hub
          google-cloud-sdk
          # haskellPackages.cabal-fmt # broken
          haskellPackages.ghcid
          haskellPackages.hadolint
          haskellPackages.hlint
          haskellPackages.hoogle
          haskellPackages.hpack
          # haskellPackages.stylish-haskell # broken
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
          solargraph
          stack
          terminal-notifier
          terraform_0_12
          tmux
          tree
          watch
          wget
        ];
      };
    };
}
