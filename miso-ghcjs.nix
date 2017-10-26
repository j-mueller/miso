{ mkDerivation, aeson, base, bytestring, containers, ghcjs-base
, network-uri, scientific, stdenv, text, transformers
, unordered-containers, vector, hspec, hspec-core, servant
, http-types, http-api-data, QuickCheck, quickcheck-instances
, lib
}:
mkDerivation {
  pname = "miso";
  version = "0.9.0.0";
  src = lib.sourceFilesBySuffices ./. [".hs" ".cabal" ".js" "LICENSE"];
  configureFlags = [ "-ftests" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers ghcjs-base network-uri scientific
    text transformers unordered-containers vector hspec hspec-core servant
    http-types http-api-data QuickCheck quickcheck-instances
  ];
  homepage = "http://github.com/dmjio/miso";
  description = "A tasty Haskell front-end framework";
  license = stdenv.lib.licenses.bsd3;
}
