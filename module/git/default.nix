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
   ".config/gh/config.yml".source = ./gh-config.yml;
   ".config/git/config".source = ./config;
   ".config/git/ignore".source = ./ignore;
  };

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
}
