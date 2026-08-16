{
  pkgs,
  lib,
  perSystem,
  ...
}:

let
  emacsclient =
    let
      alternative = lib.optionalString pkgs.stdenv.hostPlatform.isDarwin "open -a emacs";
    in
    "emacsclient --create-frame --alternate-editor='${alternative}'";
in
{
  # for
  #   - git committing
  #   - less v
  home.sessionVariables.EDITOR = emacsclient;

  programs.fish = {
    shellAliases = {
      e = "${emacsclient} --no-wait";
      ekill = "emacsclient --eval '(kill-emacs)'";
    };
    # https://dakra.github.io/ghostel/#shell-integration
    shellInit = ''
      string match --quiet --regex '^ghostel(,|$)' -- "$INSIDE_EMACS";
      and source "$EMACS_GHOSTEL_PATH/etc/shell/ghostel.fish"
    '';
  };

  home.packages = [ perSystem.self.emacs ];

  xdg.configFile."emacs/init.el".source = ./init.el;
}
