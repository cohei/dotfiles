{ hostName, config, ... }:

{
  home-manager = {
    extraSpecialArgs = { inherit hostName; };
    useGlobalPkgs = false;
  };

  nix = {
    gc = {
      automatic = true;
      options = "--delete-older-than 7d";
    };
    optimise.automatic = true;
    registry.nixpkgs-unstable.to = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixpkgs-unstable";
    };
    settings.experimental-features = "nix-command flakes";
  };

  programs.fish.enable = true;
  environment.shells = [ config.programs.fish.package ];

  security.pam.services.sudo_local.touchIdAuth = true;

  system.stateVersion = 6;
}
