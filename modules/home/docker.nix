{ ... }:

{
  home.sessionPath = [ "$HOME/.docker/bin" ];

  home.sessionVariables.COMPOSE_BAKE = "true";

  programs.fish.shellAbbrs.doco = "docker compose";
}
