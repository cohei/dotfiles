{
  description = "My Home";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, home-manager, utils }:
    let
      isDarwin = system: (import nixpkgs { inherit system; }).stdenv.isDarwin;

      homeDirectory = isDarwin: username: "/${if isDarwin then "Users" else "home"}/${username}";

      homeManagerConfiguration = system: username:
        home-manager.lib.homeManagerConfiguration {
          configuration = ./home.nix;
          inherit system username;
          homeDirectory = homeDirectory (isDarwin system) username;
        };
    in
      utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin" "aarch64-linux"] (system: {
        apps.home-manager = {
          type = "app";
          program = "${home-manager.defaultPackage.${system}}/bin/home-manager";
        };
      }) // {
        homeConfigurations = {
          "root@testcontainer" = homeManagerConfiguration "x86_64-linux" "root";
          hoge = homeManagerConfiguration "x86_64-linux" "hoge";
        };
      };
}
