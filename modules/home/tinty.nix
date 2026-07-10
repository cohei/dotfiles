{ inputs, pkgs, ... }:

{
  home.packages = [ pkgs.tinty ];

  programs.alacritty.settings =
    let
      themeFile = "${inputs.tinted-terminal}/themes/alacritty/base16-solarized-dark.toml";
      theme = fromTOML (builtins.readFile themeFile);
    in
    {
      general.import = [ themeFile ];
      bell.color = theme.colors.normal.magenta; # Solarized violet
    };
}
