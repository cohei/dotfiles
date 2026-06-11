{ hostName, config, lib, ... }:

{
  imports = [
    ./llm-agents.nix
  ];

  environment = {
    # nix-darwin doesn't add the XDG profile path even with
    # use-xdg-base-directories; without this `nix profile` installs
    # land outside PATH. See https://github.com/nix-darwin/nix-darwin/issues/943.
    profiles = lib.mkOrder 800 [ "$HOME/.local/state/nix/profile" ];
    shells = [ config.programs.fish.package ];
  };

  home-manager = {
    extraSpecialArgs = { inherit hostName; };
    useGlobalPkgs = false;
  };

  homebrew = {
    enable = true;
    enableFishIntegration = true;
    global.brewfile = true;
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
    settings = {
      experimental-features = "nix-command flakes";
      trusted-users = [ "@admin" ];
      use-xdg-base-directories = true;
    };
  };

  programs.fish.enable = true;

  security.pam.services.sudo_local.touchIdAuth = true;

  system.stateVersion = 6;
}
