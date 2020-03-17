let
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
in
{
  packageOverrides = pkgs:
    with pkgs;
    let
      enhancd = import ./enhancd.nix { inherit stdenv fetchFromGitHub runtimeShell; };
    in {
      myPackages = buildEnv {
        name = "my-packages";
        paths = [
          (all-hies.selection { selector = p: { inherit (p) ghc882; }; })
          bashCompletion
          bashInteractive_5
          cacert # not to cause SSL error
          cachix
          cloc
          cmake
          coreutils
          direnv
          emacs
          enhancd
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
          haskellPackages.cabal-fmt
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
          nix
          nix-bash-completions
          nkf
          parallel
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
        ];
      };
    };
}
