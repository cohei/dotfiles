{
  pkgs,
  flake,
  perSystem,
  hostName ? throw "home-manager-news: provide hostName via override",
  username ? throw "home-manager-news: provide username via override",
}:

pkgs.writeShellApplication {
  name = "home-manager-news";
  runtimeInputs = [ perSystem.home-manager.home-manager ];
  # Broken when built from a dirty tree: `flake` is then a raw worktree
  # copy (including .git/, for example) that Nix cannot evaluate as a flake.
  text = ''
    home-manager news --flake "${flake}#${username}@${hostName}"
  '';
}
