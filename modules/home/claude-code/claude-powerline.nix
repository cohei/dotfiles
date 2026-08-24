{ pkgs, ... }:

{
  xdg.configFile."claude-powerline/config.json".source =
    let
      writeOrderedJSON =
        name: makeValue:
        let
          ordered = entries: { __ordered = entries; };
        in
        pkgs.runCommand name
          {
            nativeBuildInputs = [ pkgs.jq ];
            json = builtins.toJSON (makeValue ordered);
            passAsFile = [ "json" ];
          }
          ''
            jq 'walk(if type == "object" and has("__ordered") then (.__ordered | add) else . end)' \
              "$jsonPath" > $out
          '';
    in
    writeOrderedJSON "claude-powerline-config.json" (ordered: {
      display.lines = [
        {
          segments = ordered [
            {
              directory = {
                enabled = true;
                style = "basename";
              };
            }
            { model.enabled = true; }
            {
              thinking = {
                enabled = true;
                showEnabled = false;
              };
            }
            {
              block = {
                enabled = true;
                displayStyle = "blocks";
              };
            }
            {
              weekly = {
                enabled = true;
                displayStyle = "blocks";
              };
            }
            {
              context = {
                enabled = true;
                showPercentageOnly = true;
                percentageMode = "used";
              };
            }
          ];
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
            magenta = "#d33682";
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
          directory = segmentColor solarized.magenta;
          model = segmentColor solarized.violet;
          thinking = segmentColor solarized.blue;
          block = segmentColor solarized.green;
          weekly = segmentColor solarized.green;
          context = segmentColor solarized.base01;
          contextWarning = segmentColor solarized.yellow;
          contextCritical = segmentColor solarized.orange;
        };
    });
}
