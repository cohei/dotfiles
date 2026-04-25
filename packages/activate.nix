{ flake, pkgs, perSystem }:

let
  options =
    if pkgs.stdenv.isDarwin
    then { input = perSystem.nix-darwin; command = "darwin-rebuild"; }
    else { input = perSystem.home-manager; command = "home-manager"; };
in
pkgs.writeShellApplication {
  name = "activate";
  runtimeInputs = [ options.input.default ];
  text = "${options.command} switch --flake ${flake}";
}
