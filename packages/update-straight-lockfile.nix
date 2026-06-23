{ pkgs }:

pkgs.writeShellApplication {
  name = "update-straight-lockfile";
  runtimeInputs = [ pkgs.emacs pkgs.home-manager pkgs.jujutsu ];
  text = ''
    lockfile=~/.config/emacs/straight/versions/default.el
    lockfile_dotfiles=./modules/home/emacs/default.el

    [ -f $lockfile ] && rm $lockfile
    emacsclient --eval '(straight-freeze-versions)'
    mv $lockfile $lockfile_dotfiles
    sudo darwin-rebuild switch
    jj commit --message 'Emacs: update straight lockfile' $lockfile_dotfiles
  '';
}
