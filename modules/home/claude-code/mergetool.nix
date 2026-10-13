{
  pkgs,
  ...
}:

{
  home.packages = [ pkgs.claude-mergetool ];

  programs.git.settings.mergetool.claude = {
    cmd = "claude-mergetool merge \"$BASE\" \"$LOCAL\" \"$REMOTE\" -o \"$MERGED\"";
    trustExitCode = true;
  };

  programs.jujutsu.settings.merge-tools.claude = {
    program = "claude-mergetool";
    merge-args = [
      "merge"
      "$base"
      "$left"
      "$right"
      "-o"
      "$output"
      "-p"
      "$path"
    ];
  };
}
