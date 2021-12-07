{ pkgs, ... }:

{
  home.packages =
    with pkgs;
    [
      git
      gitAndTools.delta
      gitAndTools.gh
    ];

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
