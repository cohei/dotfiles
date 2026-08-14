{
  lib,
  pkgs,
  perSystem,
  ...
}:

let
  jj-navi = perSystem.self.jj-navi;

  shim = command: ''
    set --local directive_file (mktemp)

    NAVI_DIRECTIVE_FILE=$directive_file command ${command} $argv
    set --local exit_code $status

    if test -s $directive_file
        source $directive_file
        set --local source_status $status
        test $exit_code -eq 0; and set exit_code $source_status
    end

    rm --force $directive_file
    return $exit_code
  '';

  completion =
    command:
    pkgs.runCommand "${command}-fish-completion" { } ''
      COMPLETE=fish ${lib.getExe' jj-navi command} > $out
    '';
in

{
  home.packages = [ jj-navi ];

  programs.fish.functions = {
    navi = shim "navi";
    nv = shim "nv";
  };

  # Avoid IFD: programs.fish.completions only takes a string, not a derivation.
  xdg.configFile = {
    "fish/completions/navi.fish".source = completion "navi";
    "fish/completions/nv.fish".source = completion "nv";
  };
}
