{ pkgs, ... }:

let
  ccusage = pkgs.writeShellApplication {
    name = "ccusage";
    runtimeInputs = [ pkgs.nodejs ];
    text = ''
      npx ccusage@latest "$@"
    '';
  };
in
{
  home.packages = [ ccusage ];
}
