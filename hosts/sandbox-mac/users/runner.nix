{ flake, ... }:

{
  imports = builtins.attrValues flake.homeModules;
  my.tup.enable = false; # macFUSE is a kernel extension and doesn't work in VM
}
