{ pkgs, lib, ... }:

let
  emacsclient =
    let
      alternative = lib.optionalString pkgs.stdenv.isDarwin "open -a emacs";
    in
      "emacsclient --create-frame --alternate-editor='${alternative}'";
in
{
  home.packages = [
    pkgs.cmake # vterm
    pkgs.emacs-lsp-booster
    pkgs.libvterm-neovim
  ];

  # for
  #   - git committing
  #   - less v
  home.sessionVariables.EDITOR = emacsclient;

  programs.fish.shellAliases = {
    e = "${emacsclient} --no-wait";
    ekill = "emacsclient --eval '(kill-emacs)'";
  };

  programs.emacs.enable = true;

  xdg.configFile = {
    "emacs/init.el".source = ./init.el;
    "emacs/straight/versions/default.el".source = ./default.el;
  };
}
