{ pkgs, pname }:

pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
  inherit pname;

  version = "5.0.3";

  src = pkgs.fetchzip {
    url = "https://github.com/miiton/Cica/releases/download/v${finalAttrs.version}/Cica_v${finalAttrs.version}.zip";
    sha256 = "sha256-BtDnfWCfD9NE8tcWSmk8ciiInsspNPTPmAdGzpg62SM=";
    stripRoot = false;
  };

  nativeBuildInputs = [ pkgs.installFonts ];

  meta = with pkgs.lib; {
    description = "Programming font mixing Hack, DejaVu Sans Mono, Rounded Mgen+ and Noto Emoji";
    homepage = "https://github.com/miiton/Cica";
    license = licenses.ofl;
    platforms = platforms.all;
  };
})
