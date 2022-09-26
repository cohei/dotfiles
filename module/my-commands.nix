{ pkgs, ... }:

let
  dotall = pkgs.writeShellApplication {
    name = "dotall";
    runtimeInputs = [ pkgs.graphviz ];
    text = ''
      find "$1" -name "*.dot" -exec dot -Tpdf -O {} \;
    '';
  };
  shell-expansion = pkgs.writeShellApplication {
    name = "shell-expansion";
    runtimeInputs = [ pkgs.coreutils ];
    text = ''
      tempfile=$(mktemp)

      {
        echo 'cat <<EOF' ; cat - ; echo 'EOF'
      } > "$tempfile"

      bash "$tempfile"

      rm "$tempfile"
    '';
  };
  touch-sudo = pkgs.writeShellApplication {
    name = "touch-sudo";
    runtimeInputs = [ pkgs.gnused ];
    text = ''
      sed --in-place=.bak '2i auth       sufficient     pam_tid.so' /etc/pam.d/sudo
    '';
  };
in
{
  home.packages = [ dotall shell-expansion touch-sudo ];
}
