{
  config,
  lib,
  pkgs,
  ...
}:

let
  # stakk starts one jj process per commit at once, which exceeds macOS's default soft limit of 256 open files.
  stakk = pkgs.symlinkJoin {
    name = "stakk";
    paths = [ pkgs.stakk ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/stakk --run 'ulimit -S -n 4096'
    '';
  };
in
{
  home = {
    packages = [ stakk ];

    file =
      let
        configDirectory =
          if pkgs.stdenv.hostPlatform.isDarwin then "Library/Application Support" else config.xdg.configHome;
        configFile = (pkgs.formats.toml { }).generate "stakk-config.toml";
      in
      {
        "${configDirectory}/stakk/config.toml".source = configFile {
          native_stacks = "auto";
          pr_mode = "draft";
        };
      };
  };

  xdg.configFile."fish/completions/stakk.fish".source = pkgs.runCommand "stakk-fish-completion" { } ''
    ${lib.getExe pkgs.stakk} completions fish > $out
  '';
}
