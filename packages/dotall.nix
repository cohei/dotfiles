{ pkgs, pname }:

pkgs.writeShellApplication {
  name = pname;
  runtimeInputs = [ pkgs.graphviz ];
  text = ''
    find "$1" -name "*.dot" -exec dot -Tpdf -O {} \;
  '';
}
