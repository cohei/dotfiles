{
  pkgs,
  lib,
  inputs,
  ...
}:

let
  emacsclient =
    let
      alternative = lib.optionalString pkgs.stdenv.isDarwin "open -a emacs";
    in
    "emacsclient --create-frame --alternate-editor='${alternative}'";

  emacs = pkgs.emacsWithPackagesFromUsePackage {
    config = ./init.el;
    override =
      final: prev:
      let
        packageRequiresFromFile =
          file:
          let
            parse-nix = "${inputs.emacs-overlay}/parse.nix";
            parse = (pkgs.callPackage parse-nix { }).parsePackagesFromPackageRequires;
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
  };
in
{
  # for
  #   - git committing
  #   - less v
  home.sessionVariables.EDITOR = emacsclient;

  programs.fish = {
    shellAliases = {
      e = "${emacsclient} --no-wait";
      ekill = "emacsclient --eval '(kill-emacs)'";
    };
    # https://dakra.github.io/ghostel/#shell-integration
    shellInit = ''
      string match --quiet --regex '^ghostel(,|$)' -- "$INSIDE_EMACS";
      and source "$EMACS_GHOSTEL_PATH/etc/shell/ghostel.fish"
    '';
  };

  home.packages = [ emacs ];

  xdg.configFile."emacs/init.el".source = ./init.el;
}
