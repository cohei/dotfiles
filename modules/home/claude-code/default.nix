{ pkgs, perSystem, ... }:

{
  imports = [ ./notify.nix ];

  programs.claude-code = {
    enable = true;
    package = pkgs.unfree.claude-code;
    context = ./context.md;
    settings = {
      effortLevel = "high";
      model = "opus";
      permissions = {
        allow = [
          "Bash(gh issue list:*)"
          "Bash(gh issue view:*)"
          "Bash(gh pr list:*)"
          "Bash(gh pr view:*)"
          "Bash(gh release view:*)"
          "Bash(gh repo view:*)"
          "Bash(jj diff:*)"
          "Bash(jj log:*)"
          "Bash(jj show:*)"
          "Bash(jj status:*)"
          "Bash(ls:*)"
          "WebFetch(domain:deepwiki.com)"
          "WebFetch(domain:gist.github.com)"
          "WebFetch(domain:github.com)"
          "WebFetch(domain:jj-vcs.github.io)"
          "WebFetch(domain:raw.githubusercontent.com)"
          "WebFetch(domain:stackoverflow.com)"
          "WebSearch"
          "mcp__serena"
        ];
      };
    };
    skills = {
      find-skills =
        let
          find-skills = pkgs.fetchFromGitHub {
            owner = "vercel-labs";
            repo = "skills";
            tag = "v1.5.1";
            hash = "sha256-JVJeottMyjxdiGPS7O4QsshKdbwbYcKMvwe/PB7I/Zw=";
          };
        in
        "${find-skills}/skills/find-skills/SKILL.md";
    };
  };

  home.packages = [
    perSystem.serena.default
    pkgs.ripgrep
    pkgs.skills
  ];
}
