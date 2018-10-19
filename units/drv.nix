{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bifunctors, fixed, formatting, numbers
      , siggy-chardust, stdenv, text, uom-plugin
      }:
      mkDerivation {
        pname = "flight-units";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [
          base bifunctors fixed formatting numbers siggy-chardust text
          uom-plugin
        ];
        homepage = "https://github.com/blockscope/flare-timing#readme";
        description = "Units used in hang gliding and paragliding competitions";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
