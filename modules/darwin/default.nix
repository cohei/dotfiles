{ hostName, ... }:

{
  home-manager = {
    extraSpecialArgs = { inherit hostName; };
    useGlobalPkgs = false;
  };

  nix.settings.experimental-features = "nix-command flakes";

  programs.fish.enable = true;

  security.pam.services.sudo_local.touchIdAuth = true;

  system.stateVersion = 6;
}
