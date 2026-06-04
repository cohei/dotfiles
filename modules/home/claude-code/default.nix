{ inputs, pkgs, perSystem, ... }:

{
  imports = [ ./notify.nix ./serena.nix ];

  programs.claude-code = {
    enable = true;
    package = perSystem.llm-agents.claude-code;
    context = ./context.md;
    settings = {
      effortLevel = "xhigh";
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
        ];
      };
    };
    skills = {
      commit = ./skills/commit;
      find-skills =
        "${inputs.vercel-skills}/skills/find-skills";
      grill-me =
        "${inputs.mattpocock-skills}/skills/productivity/grill-me";
      skill-creator =
        "${inputs.anthropics-skills}/skills/skill-creator";
    };
  };

  home.packages = [
    perSystem.llm-agents.ccusage
    perSystem.llm-agents.skills
    pkgs.ripgrep
  ];
}
