{ config, lib, pkgs, ... }:

{
  options.my.tup.enable = lib.mkOption {
    type = lib.types.bool;
    default = true;
    description = "Whether to install tup.";
  };

  config = lib.mkIf config.my.tup.enable {
    # broken on Darwin in current nixpkgs
    home.packages = [ pkgs.for-tup.tup ];
  };
}
