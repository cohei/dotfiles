{ pkgs, perSystem, ... }:

{
  programs.claude-code = {
    enable = true;
    package = pkgs.unfree.claude-code;
    memory.source = ./memory.md;
    settings = {
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
          "WebFetch(domain:deepwiki.com)"
          "WebFetch(domain:gist.github.com)"
          "WebFetch(domain:github.com)"
          "WebFetch(domain:jj-vcs.github.io)"
          "WebFetch(domain:raw.githubusercontent.com)"
          "WebFetch(domain:stackoverflow.com)"
          "WebSearch"
          "mcp__serena__*"
        ];
      };
    };
  };

  home.packages = [
    pkgs.ripgrep
    perSystem.serena.default
  ];
}
