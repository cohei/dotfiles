{ flake, ... }:

{
  imports = builtins.attrValues flake.homeModules;
}
