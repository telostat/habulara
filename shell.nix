{ compiler ? "ghc902"
, ...
}:

let
  ## Import sources:
  sources = import ./nix/sources.nix;

  ## Pinned nixpkgs:
  pkgs = import sources.nixpkgs { };

  ## Get the haskell set:
  haskell = pkgs.haskell.packages.${compiler};

  ## Get this package:
  thisPackage = haskell.callCabal2nixWithOptions "habulara" ./. "--no-haddock" { };

  ## Get this package's Haskell dependencies:
  thisPackageDeps = pkgs.haskell.lib.compose.getHaskellBuildInputs thisPackage;

  ## Get our GHC for development:
  ghc = haskell.ghcWithPackages (_: thisPackageDeps);
in
pkgs.mkShell {
  buildInputs = [
    ## Fancy stuff:
    pkgs.figlet
    pkgs.lolcat

    ## Release stuff:
    pkgs.busybox
    pkgs.gh
    pkgs.git
    pkgs.git-chglog
    pkgs.upx

    ## Haskell stuff:
    ghc
    pkgs.cabal-install
    pkgs.cabal2nix
    pkgs.haskell-language-server
    pkgs.haskellPackages.apply-refact
    pkgs.hlint
  ];

  shellHook = ''
    figlet -w 999 "HABULARA DEV SHELL" | lolcat -S 42
  '';
}
