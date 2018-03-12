{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, constraint-classes, hmatrix, newtype
      , openblasCompat, phantom-index, semigroups, stdenv, vector
      }:
      mkDerivation {
        pname = "LATS";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [
          base constraint-classes hmatrix newtype phantom-index semigroups
          vector
        ];
        librarySystemDepends = [ openblasCompat ];
        homepage = "http://github.com/guaraqe/LATS#readme";
        description = "Linear Algebra on Typed Spaces";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
