{ mkDerivation, base, constraint-classes, hmatrix, newtype
, openblasCompat, phantom-index, semigroups, stdenv, vector
}:
mkDerivation {
  pname = "LATS";
  version = "0.5.0";
  src = ./.;
  libraryHaskellDepends = [
    base constraint-classes hmatrix newtype phantom-index semigroups
    vector
  ];
  librarySystemDepends = [ openblasCompat ];
  homepage = "http://github.com/guaraqe/LATS#readme";
  description = "Linear Algebra on Typed Spaces";
  license = stdenv.lib.licenses.bsd3;
}
