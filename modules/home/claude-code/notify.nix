{ pkgs, lib, ... }:

let
  claude-code-notify = pkgs.writeShellApplication {
    name = "claude-code-notify";
    runtimeInputs = [ pkgs.terminal-notifier ];
    text = ''
      terminal-notifier \
        -title 'Claude Code' \
        -subtitle "$(basename "$CLAUDE_PROJECT_DIR")" \
        -message "$1" \
        -sound default \
        -activate org.alacritty
    '';
  };
in
{
  config = lib.mkIf pkgs.stdenv.isDarwin {
    home.packages = [ claude-code-notify ];

    programs.claude-code.settings.hooks = {
      Notification = [{
        matcher = "";
        hooks = [{
          type = "command";
          command = "jq --raw-output .message | xargs -I {} claude-code-notify {}";
        }];
      }];
    };
  };
}
