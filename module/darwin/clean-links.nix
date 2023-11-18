{ stdenv, lib, fetchzip }:

stdenv.mkDerivation rec {
  pname = "clean-links";

  version = "v1.2+13";

  src = fetchzip {
    url = "https://github.com/Sh1d0w/clean-links/releases/download/${version}/Clean.Links.app.zip";
    sha256 = "sha256-ArNT7obOvxzxzhPpvIPo/weV8opn9naojmd9L5l0Tqc=";
    stripRoot = false;
  };

  installPhase = ''
    mkdir -p $out/Applications
    cp -r 'Clean Links.app' $out/Applications/
  '';

  meta = with lib; {
    homepage = "https://github.com/Sh1d0w/clean-links";
    license = licenses.mit;
    platforms  = platforms.darwin;
  };
}
