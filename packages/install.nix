{ pkgs, perSystem }:

pkgs.writeShellApplication {
  name = "install";
  runtimeInputs = [ perSystem.home-manager.default ];
  text = ''
    home-manager switch --flake "''${1:-github:cohei/dotfiles}"
  '';
}
