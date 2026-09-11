{
  config,
  lib,
  pkgs,
  ...
}:

{
  home = {
    packages = [ pkgs.stakk ];

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
