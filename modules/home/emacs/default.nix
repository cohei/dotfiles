{ pkgs, lib, config, ... }:

let
  emacsclient =
    let
      alternative = lib.optionalString pkgs.stdenv.isDarwin "open -a emacs";
    in
      "emacsclient --create-frame --alternate-editor='${alternative}'";

  update-straight-lockfile = pkgs.writeShellApplication {
    name = "update-straight-lockfile";
    runtimeInputs = [ config.programs.emacs.package pkgs.home-manager pkgs.jujutsu ];
    text = ''
      lockfile=~/.config/emacs/straight/versions/default.el
      lockfile_dotfiles=./modules/home/emacs/default.el

      [ -f $lockfile ] && rm $lockfile
      emacsclient --eval '(straight-freeze-versions)'
      mv $lockfile $lockfile_dotfiles
      sudo darwin-rebuild switch
      jj commit --message 'Emacs: update straight lockfile' $lockfile_dotfiles
    '';
  };
in
{
  home.packages = [
    pkgs.cmake # vterm
    pkgs.emacs-lsp-booster
    pkgs.libvterm-neovim
    update-straight-lockfile
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
