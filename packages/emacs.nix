{
  inputs,
  pkgs,
}:

let
  inherit (pkgs) lib;

  pkgsWithEmacsOverlay = pkgs.extend inputs.emacs-overlay.overlays.default;
in
pkgsWithEmacsOverlay.emacsWithPackagesFromUsePackage {
  config = ../modules/home/emacs/init.el;
  override =
    final: prev:
    let
      packageRequiresFromFile =
        file:
        let
          parse-nix = "${inputs.emacs-overlay}/parse.nix";
          parse = (pkgsWithEmacsOverlay.callPackage parse-nix { }).parsePackagesFromPackageRequires;
        in
        map (name: final.${name}) (lib.remove "emacs" (parse (builtins.readFile file)));
      packageRequiresFor =
        finalAttrs: final.packageRequiresFromFile "${finalAttrs.src}/${finalAttrs.ename}.el";

      fromInput =
        pname:
        final.melpaBuild (finalAttrs: {
          inherit pname;
          src = inputs.${pname};
          version = finalAttrs.src.lastModifiedDate;
          packageRequires = final.packageRequiresFor finalAttrs;
        });
    in
    {
      inherit packageRequiresFromFile packageRequiresFor;
    }
    // lib.genAttrs [
      "auto-side-windows"
      "balanced-windows"
      "claude-code-ide"
      # The latest ELPA release, 0.5, still uses `git_head`, which jj 0.43 removed.
      # Build from the input until a newer release is out.
      "vc-jj"
    ] fromInput;
  extraEmacsPackages = epkgs: [
    (epkgs.treesit-grammars.with-grammars (
      p: with p; [
        tree-sitter-bash
        tree-sitter-css
        tree-sitter-dockerfile
        tree-sitter-elisp
        tree-sitter-go
        tree-sitter-gomod
        tree-sitter-haskell
        tree-sitter-html
        tree-sitter-java
        tree-sitter-javascript
        tree-sitter-json
        tree-sitter-nix
        tree-sitter-ruby
        tree-sitter-rust
        tree-sitter-toml
        tree-sitter-yaml
      ]
    ))
  ];
}
