{ pkgs, pname }:

pkgs.writers.writeHaskellBin
  pname
  { libraries = [ pkgs.haskellPackages.time ]; }
  ./Main.hs
