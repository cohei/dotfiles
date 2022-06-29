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
    in
      utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin"] (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          isDarwin = pkgs.stdenv.isDarwin;

          homeManagerConfiguration = username:
            home-manager.lib.homeManagerConfiguration {
              inherit pkgs;
              modules = [
                ./home.nix
                {
                  home = {
                    inherit username;
                    homeDirectory = "/${if isDarwin then "Users" else "home"}/${username}";
                    stateVersion = "22.05";
                  };
                }
              ] ++ modules;
            };

        in {
          apps.home-manager = utils.lib.mkApp { drv = home-manager.defaultPackage.${system}; };

          packages.homeConfigurations = {
            "root@testcontainer" = homeManagerConfiguration "root";
            hoge = homeManagerConfiguration "hoge";
          };
        });
}
