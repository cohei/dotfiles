{
  description = "My Home";

  inputs = {
    blueprint = {
      url = "github:numtide/blueprint";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    jj-spr = {
      url = "github:LucioFranco/jj-spr";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "nixpkgs";
    nixpkgs-for-tup.url = "github:NixOS/nixpkgs/0d00f23f023b7215b3f1035adb5247c8ec180dbc";
    nixpkgs-unfree = {
      url = "github:numtide/nixpkgs-unfree";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    serena = {
      url = "github:oraios/serena";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    starship-jj = {
      url = "gitlab:lanastara_foss/starship-jj";
      inputs.fenix.follows = "jj-spr/fenix";
      inputs.flake-utils.follows = "serena/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.systems.follows = "blueprint/systems";
    };
  };

  outputs = inputs: inputs.blueprint { inherit inputs; };
}
