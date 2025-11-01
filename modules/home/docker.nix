{ pkgs, ... }:

{
  home.sessionPath = [ "$HOME/.docker/bin" ];

  home.sessionVariables.COMPOSE_BAKE = "true";

  programs.fish.shellAbbrs.doco = "docker compose";

  xdg.configFile."fish/completions/docker.fish".source =
    pkgs.runCommand "docker-fish-completion" { buildInputs = [ pkgs.docker ]; }
      ''
        docker completion fish > $out
      '';
}
