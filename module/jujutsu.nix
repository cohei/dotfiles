{ ... }:

{
  programs.jujutsu = {
    enable = true;
    settings = {
      aliases = {
        d = ["diff"];
        s = ["status"];
        l = ["log" "--template" "my_log_oneline"];
        la = ["l" "--revisions" "all()"];
      };
      template-aliases.my_log_oneline = ''
        if(root,
          builtin_log_root(change_id, commit_id),
          label(if(current_working_copy, "working_copy"),
            concat(
              separate(" ",
                builtin_change_id_with_hidden_and_divergent_info,
                if(description, description.first_line(), description_placeholder),
                if(empty, label("empty", "(empty)")),
                surround("(", ")",
                  concat(
                    separate(", ",
                      branches,
                      tags,
                      working_copies,
                      git_head,
                      format_short_commit_id(commit_id)
                    )
                  )
                ),
                if(conflict, label("conflict", "conflict")),
              ) ++ "\n",
            ),
          )
        )
      '';
      ui = {
        default-command = "l";
        diff.format = "git";
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
      jjgrep = "grep $argv[1] (jj files $argv[2])";
    };
    shellAbbrs = {
      j = "jj";
      js = "jj status";
    };
    shellAliases = {
      glgj = "git log --graph --oneline --decorate --exclude 'refs/jj/*' --all";
    };
  };
}
