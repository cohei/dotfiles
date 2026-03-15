{ pkgs, pname }:

pkgs.stdenv.mkDerivation {
  name = pname;

  src = ./.;

  buildInputs = [ pkgs.ghc ];

  buildPhase = ''
    ghc -O2 xdg-files.hs
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp xdg-files $out/bin/
  '';
}
