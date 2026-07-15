{ lib, pkgs, ... }:

{
  programs.starship = {
    enable = true;
    extraPackages = [ pkgs.starship-jj ];
    settings = {
      format =
        let
          modules = lib.concatStrings [
            "$username"
            "$hostname"
            "$localip"
            "$shlvl"
            "$singularity"
            "$kubernetes"
            "$nats"
            "$directory"
            "$vcsh"
            "\${custom.jj}"
            "\${custom.git}"
            "$all"
          ];
        in
        "[${modules}](bright-black)";
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
      git_branch.disabled = true;
      git_commit.disabled = true;
      git_state.disabled = true;
      git_status.disabled = true;
      haskell.style = "white";
      java.style = "white";
      nix_shell = {
        style = "white";
        heuristic = true;
      };
      nodejs.style = "white";
      purescript.style = "white";
      ruby.style = "white";
      scala.style = "white";
      terraform.style = "white";
      custom.git = {
        command = "STARSHIP_CONFIG=$HOME/.config/starship-git.toml starship prompt";
        require_repo = true;
        when = "! jj --ignore-working-copy root";
        format = "$output ";
      };
      custom.jj = {
        command = "starship-jj --ignore-working-copy starship prompt";
        when = "jj --ignore-working-copy root";
        format = "on $output";
      };
    };
  };

  xdg.configFile = {
    "starship-git.toml".source = ./starship-git.toml;
    "starship-jj/starship-jj.toml".source = ./starship-jj.toml;
  };
}
