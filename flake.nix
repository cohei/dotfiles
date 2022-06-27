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
          isDarwin = (import nixpkgs { inherit system; }).stdenv.isDarwin;

          homeDirectory = username: "/${if isDarwin then "Users" else "home"}/${username}";

          homeManagerConfiguration = username:
            home-manager.lib.homeManagerConfiguration {
              configuration = ./home.nix;
              inherit system username;
              homeDirectory = homeDirectory username;
              extraModules = modules;
              # This value determines the Home Manager release that your
              # configuration is compatible with. This helps avoid breakage
              # when a new Home Manager release introduces backwards
              # incompatible changes.
              #
              # You can update Home Manager without changing this value. See
              # the Home Manager release notes for a list of state version
              # changes in each release.
              stateVersion = "22.05";
            };

        in {
          apps.home-manager = utils.lib.mkApp { drv = home-manager.defaultPackage.${system}; };

          packages.homeConfigurations = {
            "root@testcontainer" = homeManagerConfiguration "root";
            hoge = homeManagerConfiguration "hoge";
          };
        });
}
