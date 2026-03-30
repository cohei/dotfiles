{ pkgs, pname }:

pkgs.writeShellApplication {
  name = pname;
  runtimeInputs = [ pkgs.coreutils ];
  text = ''
    tempfile=$(mktemp)

    {
      echo 'cat <<EOF' ; cat - ; echo 'EOF'
    } > "$tempfile"

    bash "$tempfile"

    rm "$tempfile"
  '';
}
