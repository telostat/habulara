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

  ## Define a function that makes this application statically compiled:
  makeAppStatic = drv: pkgs.haskell.lib.compose.overrideCabal
    (_: {
      enableSharedExecutables = false;
      enableSharedLibraries = false;
      doCheck = false;
      configureFlags = [
        "--ghc-option=-optl=-static"
        "--ghc-option=-optl=-pthread"
        # "--ghc-option=-fPIC"
        "--extra-lib-dirs=${pkgs.glibc.static}/lib"
        "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
        "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
      ];
    })
    drv;

  ## Get this package:
  thisPackage = haskell.callCabal2nixWithOptions "habulara" ./. "--no-check --no-haddock" { };
in
if doStatic
then makeAppStatic thisPackage
else pkgs.haskell.lib.justStaticExecutables thisPackage
