{ config, ... }:

{
  programs.jujutsu = {
    enable = true;
    settings = {
      aliases = {
        d = ["diff"];
        l = ["log" "--template" "my_log_oneline"];
        la = ["l" "--revisions" "all()"];
        showp = ["show" "@-"];
      };
      template-aliases.my_log_oneline = ''
        if(root,
          format_root_commit(self),
          label(if(current_working_copy, "working_copy"),
            concat(
              separate(" ",
                format_short_change_id_with_hidden_and_divergent_info(self),
                if(description, description.first_line(), description_placeholder),
                if(empty, label("empty", "(empty)")),
                surround("(", ")",
                  concat(
                    separate(", ",
                      bookmarks,
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
      jjgrep = "grep $argv[1] (jj file list $argv[2])";
      d = "if jj &> /dev/null; jj show $argv; else; git d $argv; end";
      s = "if jj &> /dev/null; jj status $argv; else; git s $argv; end";
    };
    shellAbbrs = {
      j = "jj";
    };
    shellAliases = {
      glgj = "git log --graph --oneline --decorate --exclude 'refs/jj/*' --all";
    };
  };
}
