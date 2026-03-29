{ pkgs, pname }:

pkgs.stdenv.mkDerivation {
  name = pname;

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
