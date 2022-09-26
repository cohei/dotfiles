{ pkgs, ... }:

let
  touch-sudo = pkgs.writeShellApplication {
    name = "touch-sudo";
    runtimeInputs = [ pkgs.gnused ];
    text = ''
      sed --in-place=.bak '2i auth       sufficient     pam_tid.so' /etc/pam.d/sudo
    '';
  };
in
{
  home.packages = [ touch-sudo ];
}
