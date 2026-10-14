{ pkgs, ... }:

{
  home.packages = with pkgs; [
    delta
    git-quick-stats
  ];

  programs = {
    fish = {
      shellAliases = {
        gap = "git ap";
        gb = "git sw (git branch | fzf | tr --delete '* ')";
        gdc = "git dc";
        gf = "git f";
        gg = "git g";
        gl = "git l";
        glg = "git lg";
        gss = "git ss";
      };
      shellInit = ''
        if type --query gh
            eval (gh completion --shell fish)
        end
      '';
    };
    git = {
      enable = true;
      ignores = [
        "/my-scratch/"

        # direnv
        ".direnv/"
        ".envrc"

        # Nix
        "/result"
        "/result-*"
      ];
      settings = {
        advice.skippedCherryPicks = false;
        alias = {
          amend = "commit --amend";
          extend = "commit --amend --no-edit";
          ap = "add --patch";
          d = "diff";
          dc = "diff --cached";
          f = "fetch --all --tags --prune --prune-tags";
          g = "grep";
          l = "log --oneline --max-count=5";
          lg = "log --graph --oneline --exclude 'refs/jj/*' --all";
          peek = "!f() { git switch --detach && git merge --no-edit origin/$1; }; f";
          rb-continue-no-edit = "-c core.editor=true rebase --continue";
          re = "restore";
          s = "status --short --branch";
          ss = "!git stash list | fzf --exit-0 --preview-window up:80% --preview 'echo {1} | tr --delete : | xargs git stash show --stat --patch --color=always' | grep --only-matching stash@{.*} | xargs git stash pop";
          sw = "switch";
          swc = "switch --no-track --create";
          swcm = ''!f() { git switch --no-track --create "$1" origin/master; }; f'';
          swd = "switch --detach";
        };
        branch.sort = "-committerdate";
        core = {
          pager = "delta";
          precomposeunicode = true;
          quotepath = false;
        };
        delta = {
          features = "diff-so-fancy";
          map-styles = "moved-color.from-style => red bold black, moved-color.from2-style => red bold black, moved-color.to-style => green bold black, moved-color.to2-style => green bold black";
          navigate = true;
          moved-color = {
            from-style = "bold magenta";
            from2-style = "bold blue";
            to-style = "bold cyan";
            to2-style = "bold yellow";
          };
        };
        diff = {
          algorithm = "histogram";
          colorMoved = "default";
          renames = "copy";
        };
        ghq.root = "~/src";
        grep.extendedRegexp = true;
        help.autocorrect = 1;
        interactive.diffFilter = "delta --color-only";
        log.date = "iso";
        merge.conflictStyle = "zdiff3";
        pull.rebase = true;
        rebase.autosquash = true;
        rerere = {
          enabled = true;
          autoUpdate = true;
        };
        stash = {
          showIncludeUntracked = true;
          showPatch = true;
        };
        user.name = "TANIGUCHI Kohei";
      };
    };
    gh = {
      enable = true;
      extensions = [ pkgs.gh-stack ];
      settings.aliases.ci-status = "!gh api repos/:owner/:repo/commits/$(git rev-parse $1)/status --jq '.statuses[] | [.state, .context, .target_url] | @tsv' | column -ts '\t' | sort";
    };
  };
}
