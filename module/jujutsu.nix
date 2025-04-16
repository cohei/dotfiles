{ config, ... }:

{
  programs.jujutsu = {
    enable = true;
    settings = {
      aliases = {
        d = ["diff"];
        l = ["log" "--template" "my_log_oneline"];
        la = ["l" "--revisions" "all()"];
        sp = ["show" "@-"];
      };
      hints.resolving-conflicts = false;
      template-aliases.my_log_oneline = ''
        label(if(current_working_copy, "working_copy"),
          separate(" ",
            format_short_change_id_with_hidden_and_divergent_info(self),
            surround("(", ")",
              concat(
                separate(", ",
                  bookmarks,
                  tags,
                  working_copies,
                  if(root, label("root", "root()")),
                  if(git_head, label("git_head", "git_head()")),
                  if(conflict, label("conflict", "conflict")),
                  if(divergent, format_short_commit_id(commit_id))
                )
              )
            ),
            if(description, description.first_line(), if(!root, description_placeholder)),
            if(!root && empty, label("empty", "(empty)"))
          ) ++ "\n"
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
    shellInit = ''
      COMPLETE=fish jj | source
    '';
  };
}
