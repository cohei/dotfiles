{ ... }:

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
        retrunk = ["rebase" "--destination" "trunk()"];
        tug = ["bookmark" "move" "--from" "heads(::@- & bookmarks())" "--to" "@-"];
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
            separate(" ",
              format_short_change_id_with_change_offset(commit),
              surround("(", ")",
                separate(", ",
                  commit.bookmarks(),
                  commit.tags(),
                  commit.working_copies(),
                  if(commit.divergent(), format_short_commit_id(commit.commit_id())),
                )
              ),
              format_commit_labels(commit),
              if(config("ui.show-cryptographic-signatures").as_boolean(),
                format_short_cryptographic_signature(commit.signature())
              ),
              if(commit.empty(), empty_commit_marker),
              if(commit.description(),
                commit.description().first_line(),
                label(if(commit.empty(), "empty"), description_placeholder),
              ),
            ) ++ "\n",
          )
        )
      '';
      ui = {
        default-command = "ls";
        diff-formatter = ":git";
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

  xdg.configFile."fish/functions/jj-workspace-open.fish".source = ./jj-workspace-open.fish;
}
