{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, blaze-html, blaze-markup
      , bytestring, containers, either, hedis, http-types, lens, parsec
      , QuickCheck, random, random-shuffle, scientific, scotty, stdenv
      , tasty, tasty-hunit, tasty-quickcheck, text, text-show
      , transformers, utf8-string, warp, wreq
      }:
      mkDerivation {
        pname = "TicTacToe";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base containers either lens parsec QuickCheck text transformers
        ];
        executableHaskellDepends = [
          aeson base blaze-html blaze-markup bytestring containers hedis
          http-types lens QuickCheck random random-shuffle scientific scotty
          tasty tasty-hunit tasty-quickcheck text text-show transformers
          utf8-string warp wreq
        ];
        description = "Initial project template from stack";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
