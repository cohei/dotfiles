{ pkgs, ... }:

{
  home.file.".config/ghc/ghci.conf".source = ./ghci.conf;

  home.packages = with pkgs; [
    haskellPackages.cabal-fmt
    haskellPackages.hoogle
    hpack
  ];
}
