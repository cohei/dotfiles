{ ... }:

{
  programs.jujutsu = {
    enable = true;
    settings = {
      aliases = {
        s = ["status"];
        la = ["log" "--revisions" "all()"];
      };
      colors = {
        rest = "black";
        "working_copy commit_id" = "blue";
        "working_copy change_id" = "magenta";
        "working_copy timestamp" = "cyan";
      };
      ui = {
        default-command = "log";
        diff.format = "git";
        pager = "delta";
      };
      user = {
        email = "a.d.xvii.kal.mai@gmail.com";
        name = "TANIGUCHI Kohei";
      };
    };
  };

  programs.fish = {
    shellAbbrs = {
      j = "jj";
    };
  };
}
