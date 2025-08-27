{ pkgs, lib, ... }:

let
  emacs = pkgs.emacs;

  update-straight-lockfile = pkgs.writeShellApplication {
    name = "update-straight-lockfile";
    runtimeInputs = [ emacs pkgs.home-manager pkgs.jujutsu ];
    text = ''
      lockfile=~/.config/emacs/straight/versions/default.el
      lockfile_dotfiles=./modules/home/emacs/default.el

      [ -f $lockfile ] && rm $lockfile
      emacsclient --eval '(straight-freeze-versions)'
      mv $lockfile $lockfile_dotfiles
      home-manager switch
      jj commit --message 'Emacs: update straight lockfile' $lockfile_dotfiles
    '';
  };
in
{
  home.packages =
    [
      emacs
      pkgs.cmake # vterm
      pkgs.emacs-lsp-booster
      pkgs.libvterm-neovim
      update-straight-lockfile
    ];

  # for
  #   - git commiting
  #   - less v
  home.sessionVariables.EDITOR =
    let
      alternative = lib.optionalString pkgs.stdenv.isDarwin "open -a emacs";
    in
      "emacsclient --create-frame --alternate-editor='${alternative}'";

  programs.fish.shellAliases = {
    e = "emacsclient --no-wait --create-frame --alternate-editor=''";
    ekill = "emacsclient --eval '(kill-emacs)'";
  };

  xdg.configFile = {
    "emacs/init.el".source = ./init.el;
    "emacs/straight/versions/default.el".source = ./default.el;
  };
}
