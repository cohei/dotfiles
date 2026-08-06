{ pkgs, ... }:

{
  home.packages = with pkgs; [
    delta
    git
    git-quick-stats
  ];

  programs = {
    fish = {
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
    gh = {
      enable = true;
      settings.aliases.ci-status =
        "!gh api repos/:owner/:repo/commits/$(git rev-parse $1)/status --jq '.statuses[] | [.state, .context, .target_url] | @tsv' | column -ts '\t' | sort";
    };
  };

  xdg.configFile = {
    "git/config".source = ./config;
    "git/ignore".source = ./ignore;
  };
}
