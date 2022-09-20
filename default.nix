{ compiler ? "ghc902"
, doStatic ? false
, ...
}:

let
  ## Import sources:
  sources = import ./nix/sources.nix;

  ## Import nixpkgs:
  pkgs = import sources.nixpkgs { };

  ## Get the haskell set:
  haskell = pkgs.haskell.packages.${compiler};

  ## Get this package:
  thisPackage = haskell.callCabal2nixWithOptions "habulara" ./. "--no-check --no-haddock" { };
in
pkgs.haskell.lib.justStaticExecutables thisPackage
