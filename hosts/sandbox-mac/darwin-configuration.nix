{ flake, ... }:

{
  imports = [ flake.darwinModules.default ];

  homebrew.user = "runner";

  nixpkgs.hostPlatform = "aarch64-darwin";
}
