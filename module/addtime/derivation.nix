{ pkgs, stdenv }:

stdenv.mkDerivation {
  name = "addtime";

  src = ./.;

  buildInputs = [
    (pkgs.haskellPackages.ghcWithPackages (p: [ p.time ]))
  ];

  buildPhase = ''
    ghc -O2 addtime.hs
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp addtime $out/bin/
  '';
}
