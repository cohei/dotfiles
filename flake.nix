{
  description = "My Home";

  inputs = {
    nixpkgs-unfree = {
      url = "github:numtide/nixpkgs-unfree";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    mac-app-util = {
      url = "github:hraban/mac-app-util";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs-for-tup = {
      url = "github:NixOS/nixpkgs/0d00f23f023b7215b3f1035adb5247c8ec180dbc";
    };
  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-unfree, home-manager, systems, flake-parts, mac-app-util, nixpkgs-for-tup }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import systems;
      perSystem = { pkgs, lib, inputs', self', system, ... }: {
        apps = {
          default = self'.apps.install;

          install = {
            type = "app";
            program = lib.getExe (pkgs.writeShellApplication {
              name = "install";
              runtimeInputs = [ inputs'.home-manager.packages.default ];
              text = ''
                home-manager switch --flake "''${1:-github:cohei/dotfiles}"
              '';
            });
          };
        };

        legacyPackages.homeConfigurations =
          lib.attrsets.genAttrs [ "root" "cohei" ] (username:
            home-manager.lib.homeManagerConfiguration (
              let
                directoryContents =
                  path: lib.attrsets.mapAttrsToList (name: _: path + "/${name}") (builtins.readDir path);
              in
                {
                  inherit pkgs;
                  modules = [ ./home.nix ] ++ directoryContents ./module;
                  extraSpecialArgs = {
                    inherit username mac-app-util;
                    inherit (pkgs.stdenv) isDarwin;
                    unfree = inputs'.nixpkgs-unfree.legacyPackages;
                    nixpkgs-for-tup = inputs'.nixpkgs-for-tup.legacyPackages;
                  };
                })
          );
      };
    };
}
