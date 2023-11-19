{ stdenv, lib, fetchzip }:

stdenv.mkDerivation rec {
  pname = "vimari";

  version = "v2.1.0";

  src = fetchzip {
    url = "https://github.com/televator-apps/vimari/releases/download/${version}/Vimari.app.zip";
    sha256 = "sha256-fsD4uNYmE93pdvom50YLJ0sS7iaVwKPTJIp8WYs1vKc=";
    stripRoot = false;
  };

  installPhase = ''
    mkdir -p $out/Applications
    cp -r Vimari.app $out/Applications/
  '';

  meta = with lib; {
    homepage = "https://televator.net/vimari/";
    license = licenses.mit;
    platforms  = platforms.darwin;
  };
}
