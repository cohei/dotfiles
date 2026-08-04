{ lib, perSystem, ... }:

let
  serena = perSystem.serena.default;
in

{
  home.packages = [ serena ];

  programs.claude-code = {
    mcpServers.serena = {
      command = lib.getExe serena;
      args = [
        "start-mcp-server"
        "--context"
        "claude-code"
        "--project-from-cwd"
      ];
    };

    settings = {
      # Hooks recommended by Serena:
      # https://oraios.github.io/serena/02-usage/030_clients.html#claude-code
      hooks = {
        PreToolUse = [
          {
            matcher = "";
            hooks = [
              {
                type = "command";
                command = "serena-hooks remind --client=claude-code";
              }
            ];
          }
          {
            matcher = "serena__.*";
            hooks = [
              {
                type = "command";
                command = "serena-hooks auto-approve --client=claude-code";
              }
            ];
          }
        ];
        SessionStart = [
          {
            matcher = "";
            hooks = [
              {
                type = "command";
                command = "serena-hooks activate --client=claude-code";
              }
            ];
          }
        ];
        SessionEnd = [
          {
            matcher = "";
            hooks = [
              {
                type = "command";
                command = "serena-hooks cleanup --client=claude-code";
              }
            ];
          }
        ];
      };
    };
  };
}
