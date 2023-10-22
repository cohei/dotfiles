{ pkgs, ... }:

{
  home.packages =
    with pkgs;
    [
      git
      gitAndTools.delta
      gitAndTools.gh
    ];

  home.file = {
   ".config/fish/functions/gh-ci-status.fish".source = ./gh-ci-status.fish;
   ".config/gh/config.yml".source = ./gh-config.yml;
   ".config/git/config".source = ./config;
   ".config/git/ignore".source = ./ignore;
  };

  programs.fish = {
    shellAliases = {
      d = "git d";
      gap = "git ap";
      gb = "git sw (git branch | fzf | tr -d '* ')";
      gdc = "git dc";
      gf = "git f";
      gg = "git g";
      gl = "git l";
      glg = "git lg";
      gss = "git ss";
      s = "git s";
    };
    shellInit = ''
      if type --query gh
          eval (gh completion --shell fish)
      end
    '';
  };
}
