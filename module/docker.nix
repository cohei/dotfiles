{ pkgs, ... }:

{
  home.file.".config/fish/completions/docker.fish".source =
    pkgs.runCommand "docker-fish-completion" { buildInputs = [ pkgs.docker ]; } ''
      docker completion fish > $out
    '';

  home.sessionPath = [ "$HOME/.docker/bin" ];

  home.sessionVariables.COMPOSE_BAKE = "true";

  programs.fish.shellAbbrs.doco = "docker compose";
}
