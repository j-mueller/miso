{ mkDerivation, aeson, base, bytestring, containers
, hspec-webdriver, http-api-data, http-types, lucid, network-uri
, servant, servant-lucid, servant-server, stdenv, text
, transformers, vector, warp, webdriver
, lib
}:
mkDerivation {
  pname = "miso";
  version = "0.9.0.0";
  src = lib.sourceFilesBySuffices ./. [".hs" ".cabal" "LICENSE"];
  configureFlags = [ "-ftests" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers http-api-data http-types lucid
    network-uri servant servant-lucid text transformers vector
  ];
  executableHaskellDepends = [
    base hspec-webdriver lucid servant servant-lucid servant-server
    warp webdriver
  ];
  homepage = "http://github.com/dmjio/miso";
  description = "A tasty Haskell front-end framework";
  license = stdenv.lib.licenses.bsd3;
}
