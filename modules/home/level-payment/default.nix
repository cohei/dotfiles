{ pkgs, ... }:

let
  level-payment = pkgs.writeShellApplication {
    name = "level-payment";
    runtimeInputs = [ pkgs.ghc ];
    text = ''
      runhaskell ${./levelPayment.hs} "$@"
    '';
  };
in
{
  home.packages = [ level-payment ];
}
