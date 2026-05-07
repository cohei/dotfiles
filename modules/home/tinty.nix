{ inputs, pkgs, ... }:

{
  home.packages = [ pkgs.tinty ];

  programs.alacritty.settings.general.import = [
    "${inputs.tinted-terminal}/themes/alacritty/base16-solarized-dark.toml"
  ];
}
