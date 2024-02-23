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
      glgj = "git log --graph --oneline --decorate --exclude 'refs/jj/*' --all";
    };
  };
}
