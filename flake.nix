{
  description = "My Home";

  inputs = {
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, home-manager, flake-utils }:
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
      flake-utils.lib.eachDefaultSystem (system: {
        apps.home-manager = flake-utils.lib.mkApp { drv = home-manager.defaultPackage.${system}; };
      }) // {
        homeConfigurations = with flake-utils.lib; {
          "root:${system.x86_64-linux}" = homeManagerConfiguration "root" system.x86_64-linux;
          "root:${system.aarch64-linux}" = homeManagerConfiguration "root" system.aarch64-linux;
          "cohei:${system.x86_64-darwin}" = homeManagerConfiguration "cohei" system.x86_64-darwin;
          "cohei:${system.aarch64-darwin}" = homeManagerConfiguration "cohei" system.aarch64-darwin;
        };
      };
}
