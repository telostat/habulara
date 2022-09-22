{ compiler ? "ghc902"
, ...
}:

let
  ## Import sources:
  sources = import ./nix/sources.nix;

  ## Pinned nixpkgs:
  pkgs = import sources.nixpkgs { };

  ## Get fourmolu source:
  source-fourmolu = builtins.fetchTarball {
    url = "https://hackage.haskell.org/package/fourmolu-0.8.2.0/fourmolu-0.8.2.0.tar.gz";
    sha256 = "0m6hi8hr1lhy3l5yrnk7dllhwmp12gpkykxb2szal5sapd9qsdh9";
  };

  ## Get the haskell set:
  haskell = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: with pkgs.haskell.lib; {
      fourmolu = super.callCabal2nixWithOptions "fourmolu" source-fourmolu "--no-check" { };
      Cabal = pkgs.haskellPackages.Cabal_3_6_3_0;
      ghc-lib-parser = pkgs.haskellPackages.ghc-lib-parser_9_2_2_20220307;
    };
  };

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

    ## Development and testing stuff:
    (pkgs.rWrapper.override
      { packages = with pkgs.rPackages; [ stringi ]; }
    )

    ## Haskell stuff:
    ghc
    haskell.fourmolu
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
