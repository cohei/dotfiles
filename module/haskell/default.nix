{ pkgs, ... }:

{
  home.packages = with pkgs; [
    haskellPackages.cabal-fmt
    haskellPackages.hoogle
    hpack
  ];

  xdg.configFile."ghc/ghci.conf".source = ./ghci.conf;
}
