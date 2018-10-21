{ mkDerivation, base, containers, deepseq, ghc, ghc-tcplugins-extra
, hlint, hpack, stdenv, tasty, tasty-hunit, template-haskell
, units-parser
}:
mkDerivation {
  pname = "uom-plugin";
  version = "0.3.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers deepseq ghc ghc-tcplugins-extra template-haskell
    units-parser
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [ base hlint tasty tasty-hunit ];
  preConfigure = "hpack";
  homepage = "https://github.com/adamgundry/uom-plugin#readme";
  description = "Units of measure as a GHC typechecker plugin";
  license = stdenv.lib.licenses.bsd3;
  buildTools = [ cabal-install ];
}
