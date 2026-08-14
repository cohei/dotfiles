{ pkgs, pname }:

pkgs.rustPlatform.buildRustPackage (finalAttrs: {
  inherit pname;

  version = "0.2.3";

  src = pkgs.fetchFromGitHub {
    owner = "eersnington";
    repo = "jj-navi";
    tag = "v${finalAttrs.version}";
    hash = "sha256-IRjQnEi32XBn/AH6vsH6DwMS/qiLsaOTOIIJb1l3XvY=";
  };

  cargoHash = "sha256-MtmVEacT9cn1wpd+bTGX/Ba2pc7rtn1ibTnt4+xlE5M=";

  nativeCheckInputs = [
    pkgs.jujutsu
    pkgs.writableTmpDirAsHomeHook
  ];

  # The tests compare against plain output, so the decoration Nix's builder pty
  # triggers has to go: NO_COLOR drops the color, TERM=dumb the hyperlink.
  preCheck = ''
    export NO_COLOR=1
    export TERM=dumb
  '';

  meta = {
    description = "Workspace orchestrator for Jujutsu, built for parallel human and AI agent workflows";
    homepage = "https://github.com/eersnington/jj-navi";
    license = pkgs.lib.licenses.mit;
    mainProgram = "navi";
    platforms = pkgs.lib.platforms.linux ++ pkgs.lib.platforms.darwin;
  };
})
