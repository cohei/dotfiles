{ hostName, config, ... }:

{
  home-manager = {
    extraSpecialArgs = { inherit hostName; };
    useGlobalPkgs = false;
  };

  nix.settings.experimental-features = "nix-command flakes";

  programs.fish.enable = true;
  environment.shells = [ config.programs.fish.package ];

  security.pam.services.sudo_local.touchIdAuth = true;

  system.stateVersion = 6;
}
