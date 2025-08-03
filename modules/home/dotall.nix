{ pkgs, ... }:

let
  dotall = pkgs.writeShellApplication {
    name = "dotall";
    runtimeInputs = [ pkgs.graphviz ];
    text = ''
      find "$1" -name "*.dot" -exec dot -Tpdf -O {} \;
    '';
  };
in
{
  home.packages = [ dotall ];
}
