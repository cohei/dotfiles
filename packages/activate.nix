{ pkgs, perSystem }:

pkgs.writeShellApplication {
  name = "activate";
  runtimeInputs = [ perSystem.home-manager.default ];
  text = ''
    home-manager switch --flake "''${1:-github:cohei/dotfiles}"
  '';
}
