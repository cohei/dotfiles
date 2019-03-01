let
# compilable in GHC 8.6.3
jlHead = {
  owner = "chrisdone";
  repo = "jl";
  rev = "3eb3d8e22e1565f17a3d73c8464b08975ff03c60";
  sha256 = "1qjg4fwpgdb4nc2x24hz7a1xcv1i8c4zhpdb0496hk792vivvj64";
};

in
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
          (jl.overrideAttrs (oldAttrs: { version = "HEAD"; src = fetchFromGitHub jlHead; }))
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
