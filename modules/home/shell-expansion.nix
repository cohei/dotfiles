{ pkgs, ... }:

let
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
in
{
  home.packages = [ shell-expansion ];
}
