{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, deepseq, ghc
      , ghc-tcplugins-extra, hlint, stdenv, tasty, tasty-hunit
      , template-haskell, units-parser, cabal-install
      }:
      mkDerivation {
        pname = "uom-plugin";
        version = "0.3.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base containers deepseq ghc ghc-tcplugins-extra template-haskell
          units-parser
        ];
        testHaskellDepends = [ base hlint tasty tasty-hunit ];
        homepage = "https://github.com/adamgundry/uom-plugin#readme";
        description = "Units of measure as a GHC typechecker plugin";
        license = stdenv.lib.licenses.bsd3;
        buildTools = [ cabal-install ];
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
