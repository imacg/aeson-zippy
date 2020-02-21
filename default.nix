{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, mtl, scientific, semigroupoids, stdenv, text
, unordered-containers, vector
}:
mkDerivation {
  pname = "aeson-zippy";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring containers mtl scientific
    semigroupoids text unordered-containers vector
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
