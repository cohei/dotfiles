{
  description = "My Home";

  inputs = {
    anthropics-skills = {
      url = "github:anthropics/skills";
      flake = false;
    };
    auto-side-windows = {
      url = "github:MArpogaus/auto-side-windows";
      flake = false;
    };
    balanced-windows = {
      url = "github:elp-revive/balanced-windows";
      flake = false;
    };
    blueprint = {
      url = "github:numtide/blueprint";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    claude-code-ide = {
      url = "github:manzaltu/claude-code-ide.el";
      flake = false;
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Not following `nixpkgs`, to keep binary cache hits.
    llm-agents.url = "github:numtide/llm-agents.nix";
    mattpocock-skills = {
      url = "github:mattpocock/skills";
      flake = false;
    };
    nix-darwin = {
      url = "github:nix-darwin/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
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
    vercel-skills = {
      url = "github:vercel-labs/skills";
      flake = false;
    };
  };

  nixConfig = {
    # cache.numtide.com advertises no priority (defaults to 0), so its priority
    # is lowered to favor cache.nixos.org (40).
    extra-substituters = [
      "https://cache.numtide.com?priority=100" # For the `llm-agents` flake.
      "https://cohei-emacs-overlay-packages.cachix.org"
    ];
    extra-trusted-public-keys = [
      "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
      "cohei-emacs-overlay-packages.cachix.org-1:vRKsq9FHm61O0lCkEkV58Zx5YL8kwdd92dUwcPMLyao="
    ];
  };

  outputs = inputs: inputs.blueprint { inherit inputs; };
}
