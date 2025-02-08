{ pkgs, ... }:

{
  home.packages = [ pkgs.tinty ];

  programs.fish.interactiveShellInit = ''
    tinty apply base16-solarized-dark
  '';
}
