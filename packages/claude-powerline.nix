{ pkgs, pname }:

pkgs.buildNpmPackage (finalAttrs: {
  inherit pname;

  version = "1.30.1";

  src = pkgs.fetchFromGitHub {
    owner = "Owloops";
    repo = "claude-powerline";
    tag = "v${finalAttrs.version}";
    hash = "sha256-Kevx+gZULenAPKe0LYov+v4byCJHoauf58Xkxd53Xyw=";
  };

  npmDepsHash = "sha256-D3Z5tb4phZUMPQaXvfYiIWuwaX5YGI8ubgyV7sSJqQk=";

  meta = {
    description = "Powerline-style statusline for Claude Code";
    homepage = "https://github.com/Owloops/claude-powerline";
    license = pkgs.lib.licenses.mit;
    mainProgram = "claude-powerline";
    platforms = pkgs.lib.platforms.all;
  };
})
