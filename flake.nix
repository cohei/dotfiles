{
  description = "My Home";

  inputs = {
    nixpkgs-unfree = {
      url = "github:numtide/nixpkgs-unfree";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    darwin-systems.url = "github:nix-systems/default-darwin";
    mac-app-util = {
      url = "github:hraban/mac-app-util";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-unfree, home-manager, systems, flake-parts, darwin-systems, mac-app-util }:
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
          let
            modules =
              builtins.map (f: ./module + ("/" + f)) (builtins.attrNames (builtins.readDir ./module));
          in
            lib.attrsets.genAttrs [ "root" "cohei" ] (username:
              home-manager.lib.homeManagerConfiguration {
                inherit pkgs;
                modules = [ ./home.nix ] ++ modules;
                extraSpecialArgs = {
                  inherit username mac-app-util;
                  unfree = inputs'.nixpkgs-unfree.legacyPackages;
                  isDarwin = builtins.elem system (import darwin-systems);
                };
              }
            );

      };
    };
}
