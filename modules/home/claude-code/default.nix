{
  inputs,
  pkgs,
  perSystem,
  ...
}:

{
  imports = [
    ./notify.nix
    ./serena.nix
  ];

  home.packages = [
    perSystem.llm-agents.ccusage
    perSystem.llm-agents.skills
    pkgs.ripgrep
  ];

  programs.claude-code = {
    enable = true;
    package = perSystem.llm-agents.claude-code;
    context = ./context.md;
    settings = {
      effortLevel = "xhigh";
      enabledPlugins."claude-powerline@claude-powerline" = true;
      extraKnownMarketplaces.claude-powerline.source = {
        source = "github";
        repo = "Owloops/claude-powerline";
      };
      model = "opus";
      permissions = {
        allow = [
          "Bash(gh issue list:*)"
          "Bash(gh issue view:*)"
          "Bash(gh pr list:*)"
          "Bash(gh pr view:*)"
          "Bash(gh release view:*)"
          "Bash(gh repo view:*)"
          "Bash(gh search:*)"
          "WebFetch(domain:deepwiki.com)"
          "WebFetch(domain:discourse.nixos.org)"
          "WebFetch(domain:docs.jj-vcs.dev)"
          "WebFetch(domain:gist.github.com)"
          "WebFetch(domain:github.com)"
          "WebFetch(domain:nix-community.github.io)"
          "WebFetch(domain:nix.dev)"
          "WebFetch(domain:nixos.org)"
          "WebFetch(domain:nixos.wiki)"
          "WebFetch(domain:raw.githubusercontent.com)"
          "WebFetch(domain:stackoverflow.com)"
          "WebFetch(domain:wiki.nixos.org)"
          "WebSearch"
        ];
        ask = [
          "Bash(dangerouslyDisableSandbox:true)"
        ];
      };
      sandbox = {
        enabled = true;
        filesystem.allowWrite = [ "~/.cache/nix" ];
        network.allowUnixSockets = [ "/nix/var/nix/daemon-socket/socket" ];
      };
      statusLine = {
        type = "command";
        command = pkgs.lib.getExe perSystem.self.claude-powerline;
      };
    };
    skills = {
      commit = ./skills/commit;
      find-skills = "${inputs.vercel-skills}/skills/find-skills";
      grill-me = "${inputs.mattpocock-skills}/skills/productivity/grill-me";
      grilling = "${inputs.mattpocock-skills}/skills/productivity/grilling";
      handoff = "${inputs.mattpocock-skills}/skills/productivity/handoff";
      skill-creator = "${inputs.anthropics-skills}/skills/skill-creator";
    };
  };

  xdg.configFile."claude-powerline/config.json".source = ./claude-powerline.json;
}
