{ pkgs, ... }:

{
  home.packages =
    with pkgs;
    [
      delta
      git
      git-quick-stats
      github-cli
    ];

  programs.fish = {
    shellAliases = {
      gap = "git ap";
      gb = "git sw (git branch | fzf | tr -d '* ')";
      gdc = "git dc";
      gf = "git f";
      gg = "git g";
      gl = "git l";
      glg = "git lg";
      gss = "git ss";
    };
    shellInit = ''
      if type --query gh
          eval (gh completion --shell fish)
      end
    '';
  };

  xdg.configFile = {
   "gh/config.yml".source = ./gh-config.yml;
   "git/config".source = ./config;
   "git/ignore".source = ./ignore;
  };
}
