{
  description = "My Home";

  inputs = {
    anthropics-skills = {
      url = "github:anthropics/skills";
      flake = false;
    };
    blueprint = {
      url = "github:numtide/blueprint";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    llm-agents = {
      url = "github:numtide/llm-agents.nix";
      inputs.blueprint.follows = "blueprint";
    };
    mattpocock-skills = {
      url = "github:mattpocock/skills";
      flake = false;
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
    tinted-terminal = {
      url = "github:tinted-theming/tinted-terminal";
      flake = false;
    };
  };

  outputs = inputs: inputs.blueprint { inherit inputs; };
}
