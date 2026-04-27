{ flake, pkgs, perSystem }:

pkgs.writeShellApplication {
  name = "activate";
  runtimeInputs = [ perSystem.home-manager.default ];
  text = "home-manager switch --flake ${flake}";
}
