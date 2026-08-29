---
name: nix
description: Conventions for writing and building Nix code. Invoke before writing or editing any `.nix` file (flakes, derivations, modules) or running a `nix` command, in any repository.
---

# Nix

## Bindings

Don't use `rec`. Bind the shared value in a `let` and refer to it, with `inherit` where the same name is reused:

```nix
let
  x48ng = pkgs.stdenv.mkDerivation { ... };
in
{
  packages.${system} = {
    inherit x48ng;
    default = x48ng;
  };
}
```

`rec` changes scope implicitly inside the attribute set, so a reader cannot tell where a name comes from. A `let` fixes each binding's origin in one place. When a derivation needs to reference itself, use `mkDerivation (finalAttrs: ...)`, not `rec`.

Name a helper function in a `let` binding too, rather than inlining it as a lambda, for example as the argument to `builtins.mapAttrs`.

## Derivations

- `finalAttrs` holds shared data (src, lists, dependencies), rather than more `let` bindings for data; the `let` holds functions
- Don't add `nativeBuildInputs`/`buildInputs` by guesswork. Build with a minimal configuration first, and add a dependency only when the build fails without it

## Commands

- Reach for the flakes commands where the docs still teach the legacy ones — `nix build .#<attr>` over `nix-build -A <attr>`, `nix develop` over `nix-shell` — since the user is standardizing on flakes
- A dirty working tree is fine; the `Git tree ... is dirty` warning is safe to ignore
- Delete the `result` symlink a `nix build` leaves in the working tree (`rm result`)
