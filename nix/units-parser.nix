{ mkDerivation, base, containers, mtl, multimap, parsec, stdenv
, syb, tasty, tasty-hunit, template-haskell
}:
mkDerivation {
  pname = "units-parser";
  version = "0.1.1.2";
  sha256 = "0a63f8b62a5d48e6c7126970cc0c6c350711b1d55ccb8182567a47ca35ce751a";
  libraryHaskellDepends = [ base containers mtl multimap parsec ];
  testHaskellDepends = [
    base containers mtl multimap parsec syb tasty tasty-hunit
    template-haskell
  ];
  doCheck = false;
  description = "A parser for units of measure";
  license = stdenv.lib.licenses.bsd3;
}
