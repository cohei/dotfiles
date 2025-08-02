{ pkgs, ... }:

let
  ccusage = pkgs.writeShellApplication {
    name = "ccusage";
    runtimeInputs = [ pkgs.bun pkgs.nodejs ];
    text = ''
      bunx ccusage "$@"
    '';
  };
in
{
  home.packages = [ ccusage ];
}
