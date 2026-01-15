{ pkgs, ... }:

{
  home.packages = with pkgs; [
    haskellPackages.cabal-gild
    haskellPackages.hoogle
  ];

  xdg.configFile."ghc/ghci.conf".source = ./ghci.conf;
}
