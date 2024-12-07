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
    let
      modules =
        builtins.map (f: ./module + ("/" + f)) (builtins.attrNames (builtins.readDir ./module));

      homeManagerConfiguration = username: system:
        home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.${system};
          modules = [
            {
              home.username = username;
              nixpkgs.overlays =
                [ (_self: _super: { unfree = nixpkgs-unfree.legacyPackages.${system}; }) ];
            }
            ./home.nix
          ] ++ modules;
          extraSpecialArgs = {
            inherit mac-app-util;
            isDarwin =
              with flake-utils.lib.system;
              system == x86_64-darwin || system == aarch64-darwin;
          };
        };
    in
      flake-utils.lib.eachDefaultSystem (system: {
        apps = rec {
          default = install;

          install = flake-utils.lib.mkApp {
            drv = nixpkgs.legacyPackages.${system}.writeShellApplication {
              name = "install";
              runtimeInputs = [ home-manager.packages.${system}.default ];
              text = ''
                home-manager switch --flake "''${1:-github:cohei/dotfiles}#''${USER}:${system}"
              '';
            };
          };
        };
      }) // {
        homeConfigurations = with flake-utils.lib.system; {
          "root:${x86_64-linux}" = homeManagerConfiguration "root" x86_64-linux;
          "root:${aarch64-linux}" = homeManagerConfiguration "root" aarch64-linux;
          "cohei:${x86_64-darwin}" = homeManagerConfiguration "cohei" x86_64-darwin;
          "cohei:${aarch64-darwin}" = homeManagerConfiguration "cohei" aarch64-darwin;
        };
      };
}
