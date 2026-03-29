{ pkgs, pname }:

pkgs.writeShellApplication {
  name = pname;
  runtimeInputs = [ pkgs.ghc ];
  text = ''
    runhaskell ${./levelPayment.hs} "$@"
  '';
}
