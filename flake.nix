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
      username = "hoge";

      isDarwin = system: (import nixpkgs { inherit system; }).stdenv.isDarwin;

      homeDirectory = isDarwin: "/${if isDarwin then "Users" else "home"}/${username}";

      homeManagerConfiguration = system:
        home-manager.lib.homeManagerConfiguration {
          configuration = ./home.nix;
          inherit system username;
          homeDirectory = homeDirectory (isDarwin system);
        };
    in
      utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin" "aarch64-linux"] (system: {
        defaultApp = self.apps.${system}.switch;

        apps.switch = {
          type = "app";
          program = "${(homeManagerConfiguration system).activationPackage}/activate";
        };
      });
}
