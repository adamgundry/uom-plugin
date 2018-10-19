{ mkDerivation, base, bifunctors, fixed, formatting, numbers
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
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/blockscope/flare-timing#readme";
  description = "Units used in hang gliding and paragliding competitions";
  license = stdenv.lib.licenses.mpl20;
}
