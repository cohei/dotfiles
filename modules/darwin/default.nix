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
    settings.experimental-features = "nix-command flakes";
  };

  programs.fish.enable = true;
  environment.shells = [ config.programs.fish.package ];

  security.pam.services.sudo_local.touchIdAuth = true;

  system.stateVersion = 6;
}
