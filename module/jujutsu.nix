{ ... }:

{
  programs.jujutsu = {
    enable = true;
    settings = {
      aliases = {
        d = ["diff"];
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
    shellAliases = {
      glgj = "git log --graph --oneline --decorate (git for-each-ref --exclude=refs/jj --format='%(refname)')";
    };
  };
}
