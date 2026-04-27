{ pkgs, pname }:

pkgs.writers.writeHaskellBin pname {} ./Main.hs
