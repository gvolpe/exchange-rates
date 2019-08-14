{ mkDerivation, aeson, base, bytestring, containers, dhall
, exceptions, hedgehog, hedis, lens, mtl, refined, rio, servant
, servant-server, servant-swagger, stdenv, swagger2
, template-haskell, text, wai, wai-cors, warp, wreq
}:
mkDerivation {
  pname = "exchange-rates";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers dhall exceptions hedis lens mtl
    refined rio servant servant-server servant-swagger swagger2 text
    wai wai-cors warp wreq
  ];
  executableHaskellDepends = [ base refined rio template-haskell ];
  testHaskellDepends = [
    base containers hedgehog mtl rio template-haskell
  ];
  homepage = "https://github.com/gvolpe/exchange-rates";
  license = stdenv.lib.licenses.asl20;
}
