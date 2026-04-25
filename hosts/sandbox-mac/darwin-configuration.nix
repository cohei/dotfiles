{ flake, ... }:

{
  imports = [ flake.darwinModules.default ];

  nixpkgs.hostPlatform = "aarch64-darwin";
}
