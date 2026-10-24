{
  lib,
  runCommand,
  starship,
}:

let
  promptOrder =
    lib.pipe
      (runCommand "starship-prompt-order" { } ''
        sed --quiet --regexp-extended \
          '/^pub const PROMPT_ORDER/,/^];$/s/^ *"([a-z_]*)",$/\1/p' \
          ${starship.src}/src/configs/starship_root.rs > $out
      '')
      [
        builtins.readFile
        (lib.removeSuffix "\n")
        (lib.splitString "\n")
      ];
  replaceVcsModules =
    let
      isVcsModule = module: builtins.match "(fossil|git|hg|pijul)_.*" module != null;
      replace =
        module:
        if isVcsModule module then
          [ ]
        else if module == "vcsh" then
          [
            "vcsh"
            "vcs"
          ]
        else
          [ module ];
    in
    lib.concatMap replace;
  toFormat = modules: "[${lib.concatMapStrings (module: "$" + module) modules}](bright-black)";
in

toFormat (replaceVcsModules promptOrder)
