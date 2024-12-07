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
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, nixpkgs-unfree, home-manager, flake-utils, mac-app-util }:
    flake-utils.lib.eachDefaultSystem (system: {
      apps = rec {
        default = install;

        install = flake-utils.lib.mkApp {
          drv = nixpkgs.legacyPackages.${system}.writeShellApplication {
            name = "install";
            runtimeInputs = [ home-manager.packages.${system}.default ];
            text = ''
              home-manager switch --flake "''${1:-github:cohei/dotfiles}"
            '';
          };
        };
      };

      legacyPackages.homeConfigurations =
        let
          modules =
            builtins.map (f: ./module + ("/" + f)) (builtins.attrNames (builtins.readDir ./module));
        in
          nixpkgs.lib.attrsets.genAttrs [ "root" "cohei" ] (username:
            home-manager.lib.homeManagerConfiguration {
              pkgs = nixpkgs.legacyPackages.${system};
              modules = [ ./home.nix ] ++ modules;
              extraSpecialArgs = {
                inherit username mac-app-util;
                unfree = nixpkgs-unfree.legacyPackages.${system};
                isDarwin =
                  with flake-utils.lib.system;
                  system == x86_64-darwin || system == aarch64-darwin;
              };
            }
          );
    });
}
