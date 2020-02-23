{ mkDerivation, aeson, attoparsec, base, containers, scientific
, stdenv, text, unordered-containers, vector
}:
mkDerivation {
  pname = "aeson-zippy";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base containers scientific text
    unordered-containers vector
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
