{ pkgs, lib, ... }:

let
  emacsclient =
    let
      alternative = lib.optionalString pkgs.stdenv.isDarwin "open -a emacs";
    in
    "emacsclient --create-frame --alternate-editor='${alternative}'";
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

  programs.emacs = {
    enable = true;
    extraPackages =
      epkgs: [
        (epkgs.treesit-grammars.with-grammars (
          p: with p; [
            tree-sitter-bash
            tree-sitter-css
            tree-sitter-dockerfile
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

  xdg.configFile = {
    "emacs/init.el".source = ./init.el;
    "emacs/straight/versions/default.el".source = ./default.el;
  };
}
