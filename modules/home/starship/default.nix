{ pkgs, ... }:

{
  programs.starship = {
    enable = true;
    settings = {
      format = pkgs.callPackage ./format.nix { };
      aws.style = "white";
      c.style = "white";
      character.success_symbol = "[❯](white)";
      cmd_duration.show_notifications = true;
      directory.style = "white";
      direnv = {
        disabled = false;
        format = "$symbol[$loaded/$allowed]($style) ";
        style = "white";
      };
      docker_context.disabled = true;
      gcloud.disabled = true;
      git_branch.style = "white";
      git_commit.style = "white";
      git_metrics.disabled = false;
      git_state.style = "bold white";
      git_status.style = "white";
      haskell.style = "white";
      java.style = "white";
      jj_bookmark.style = "white";
      jj_change.prefix_style = "bold 17";
      jj_metrics = {
        added_style = "green";
        deleted_style = "red";
      };
      jj_status.style = "white";
      nix_shell = {
        style = "white";
        heuristic = true;
      };
      nodejs.style = "white";
      pijul_channel = {
        disabled = false;
        style = "white";
      };
      purescript.style = "white";
      ruby.style = "white";
      scala.style = "white";
      terraform.style = "white";
      vcs.order = [
        "jj"
        "pijul"
        "git"
        "hg"
        "fossil"
      ];
    };
  };
}
