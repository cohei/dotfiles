{ config, ... }:

{
  programs.jujutsu = {
    enable = true;
    settings = {
      aliases = {
        d = ["diff"];
        grep = ["util" "exec" "--" "fish" "--command" "grep $argv[1] (jj file list $argv[2])"];
        l = ["log" "--template" "my_log_oneline"];
        la = ["l" "--revisions" "all()"];
        ls = ["l" "--limit" "20"];
        p = ["show" "@-"];
      };
      hints.resolving-conflicts = false;
      template-aliases.my_log_oneline = "my_log_oneline(self)";
      template-aliases."my_log_oneline(commit)" = ''
        if(commit.root(),
          format_root_commit(commit),
          label(
            separate(" ",
              if(commit.current_working_copy(), "working_copy"),
              if(commit.immutable(), "immutable", "mutable"),
              if(commit.conflict(), "conflicted"),
            ),
            concat(
              separate(" ",
                format_short_change_id_with_hidden_and_divergent_info(commit),
                surround("(", ")",
                  separate(", ",
                    commit.bookmarks(),
                    commit.tags(),
                    commit.working_copies(),
                    if(commit.git_head(), label("git_head", "git_head()")),
                    if(commit.divergent(), format_short_commit_id(commit.commit_id())),
                    if(commit.conflict(), label("conflict", "conflict"))
                  )
                ),
                if(commit.empty(), label("empty", "(empty)")),
                if(commit.description(),
                  commit.description().first_line(),
                  label(if(commit.empty(), "empty"), description_placeholder),
                ),
              ) ++ "\n",
            ),
          )
        )
      '';
      ui = {
        default-command = "ls";
        diff-formatter = ":git";
        graph.style = "square";
        log-synthetic-elided-nodes = true;
        pager = "delta";
      };
      user = {
        email = "a.d.xvii.kal.mai@gmail.com";
        name = "TANIGUCHI Kohei";
      };
    };
  };

  programs.fish = {
    functions = {
      d = "if jj &> /dev/null; jj show $argv; else; git d $argv; end";
      s = "if jj &> /dev/null; jj status $argv; else; git s $argv; end";
    };
    shellAbbrs = {
      j = "jj";
      jl = "jj l";
    };
    shellInit = ''
      COMPLETE=fish jj | source
    '';
  };
}
