{ ... }:

{
  home-manager.useGlobalPkgs = false;

  nix.settings.experimental-features = "nix-command flakes";

  programs.fish.enable = true;

  system.stateVersion = 6;
}
