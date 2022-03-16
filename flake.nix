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
      isDarwin = system: (import nixpkgs { inherit system; }).stdenv.isDarwin;

      homeDirectory = isDarwin: username: "/${if isDarwin then "Users" else "home"}/${username}";

      modules =
        builtins.map (f: ./module + ("/" + f)) (builtins.attrNames (builtins.readDir ./module));

      homeManagerConfiguration = system: username:
        home-manager.lib.homeManagerConfiguration {
          configuration = ./home.nix;
          inherit system username;
          homeDirectory = homeDirectory (isDarwin system) username;
          extraModules = modules;
          # This value determines the Home Manager release that your
          # configuration is compatible with. This helps avoid breakage
          # when a new Home Manager release introduces backwards
          # incompatible changes.
          #
          # You can update Home Manager without changing this value. See
          # the Home Manager release notes for a list of state version
          # changes in each release.
          stateVersion = "21.11";
        };
    in
      utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin"] (system: {
        apps.home-manager = utils.lib.mkApp { drv = home-manager.defaultPackage.${system}; };

        packages.homeConfigurations = {
          "root@testcontainer" = homeManagerConfiguration system "root";
          hoge = homeManagerConfiguration system "hoge";
        };
      });
}
