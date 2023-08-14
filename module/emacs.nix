{ pkgs, ... }:

{
  home.packages =
    with pkgs;
    [
      cmake # vterm
      cmigemo
      emacs29
      libvterm-neovim
    ];

  # for
  #   - git commiting
  #   - less v
  home.sessionVariables.EDITOR =
    let
      options =
        if pkgs.stdenv.isDarwin
        then "--alternate-editor='open -a emacs'"
        else "--alternate-editor='' --create-frame";
    in "emacsclient ${options}";

  programs.fish.shellAliases = {
    e = "emacsclient --no-wait --create-frame --alternate-editor=''";
    ekill = "emacsclient --eval '(kill-emacs)'";
  };
}
