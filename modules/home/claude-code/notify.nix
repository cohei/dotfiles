{ pkgs, lib, ... }:

let
  claude-code-notify = pkgs.writeShellApplication {
    name = "claude-code-notify";
    runtimeInputs = [ pkgs.terminal-notifier ];
    text = ''
      if [ -z "''${INSIDE_EMACS-}" ]; then
        app=org.alacritty
      else
        app=org.gnu.Emacs
      fi

      terminal-notifier \
        -title 'Claude Code' \
        -subtitle "$(basename "$CLAUDE_PROJECT_DIR")" \
        -message "$1" \
        -sound default \
        -activate "$app"
    '';
  };
in
{
  config = lib.mkIf pkgs.stdenv.isDarwin {
    home.packages = [ claude-code-notify ];

    programs.claude-code.settings.hooks = {
      Notification = [
        {
          matcher = "";
          hooks = [
            {
              type = "command";
              command = "jq --raw-output .message | xargs -I {} claude-code-notify {}";
            }
          ];
        }
      ];
    };
  };
}
