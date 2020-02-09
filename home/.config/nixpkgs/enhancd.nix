{ stdenv, fetchFromGitHub, runtimeShell }:

stdenv.mkDerivation {
  name = "enhancd";
  src = fetchFromGitHub {
    owner = "b4b4r07";
    repo = "enhancd";
    rev = "master";
    sha256 = "1qk2fa33jn4j3xxaljmm11d6rbng6d5gglrhwavb72jib4vmkwyb";
  };
  phases = "unpackPhase installPhase";
  installPhase = ''
    mkdir -p $out
    cp -r * $out/
    chmod +x $out/init.sh

    mkdir -p $out/bin

    cat <<SCRIPT > $out/bin/enhancd-dir
    #!${runtimeShell}
    # # Run this script to find the enhancd folder where all the shell
    # integration scripts are living.
    echo $out
    SCRIPT

    chmod +x $out/bin/enhancd-dir
  '';

  meta = {
    description = "🚀 A next-generation cd command with your interactive filter";
    homepage = "https://github.com/b4b4r07/enhancd";
    license = stdenv.lib.licenses.mit;
    platforms = stdenv.lib.platforms.all;
  };
}
