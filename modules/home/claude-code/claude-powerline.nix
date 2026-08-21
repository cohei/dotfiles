{ pkgs, ... }:

{
  xdg.configFile."claude-powerline/config.json".source =
    pkgs.writers.writeJSON "claude-powerline-config.json"
      {
        display.lines = [
          {
            segments = {
              model.enabled = true;
              thinking = {
                enabled = true;
                showEnabled = false;
              };
              block = {
                enabled = true;
                displayStyle = "blocks";
              };
              weekly = {
                enabled = true;
                displayStyle = "blocks";
              };
              context = {
                enabled = true;
                showPercentageOnly = true;
                percentageMode = "used";
              };
            };
          }
        ];

        modelContextLimits = {
          default = 1000000;
          sonnet = 1000000;
          opus = 1000000;
        };

        theme = "custom";

        colors.custom =
          let
            solarized = {
              base01 = "#586e75";
              base02 = "#073642";
              yellow = "#b58900";
              orange = "#cb4b16";
              violet = "#6c71c4";
              blue = "#268bd2";
              green = "#859900";
            };

            segmentColor = fg: {
              inherit fg;
              bg = solarized.base02;
            };
          in
          {
            model = segmentColor solarized.violet;
            thinking = segmentColor solarized.blue;
            block = segmentColor solarized.green;
            weekly = segmentColor solarized.green;
            context = segmentColor solarized.base01;
            contextWarning = segmentColor solarized.yellow;
            contextCritical = segmentColor solarized.orange;
          };
      };
}
