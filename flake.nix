{
  description = "My Home";

  inputs = {
    nixpkgs.url = "nixpkgs";

    home-manager = {
      url = "home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    utils.url = "flake-utils";
  };

  outputs = { self, nixpkgs, home-manager, utils }:
    let
      modules =
        builtins.map (f: ./module + ("/" + f)) (builtins.attrNames (builtins.readDir ./module));

      homeManagerConfiguration = username: system:
        home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.${system};
          modules = [
            { home.username = username; }
            ./home.nix
          ] ++ modules;
        };
    in
      utils.lib.eachDefaultSystem (system: {
        apps.home-manager = utils.lib.mkApp { drv = home-manager.defaultPackage.${system}; };
      }) // {
        homeConfigurations = with utils.lib; {
          "root@testcontainer" = homeManagerConfiguration "root" system.x86_64-linux;
          hoge = homeManagerConfiguration "hoge" system.aarch64-darwin;
        };
      };
}
