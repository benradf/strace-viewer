{ mkDerivation, attoparsec, base, binary, bytestring, containers
, direct-sqlite, directory, http-client, http-types, lib, process
, random, text, time, unix, wai, warp
}:
mkDerivation {
  pname = "code";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring direct-sqlite directory http-client
    http-types process text time unix wai warp
  ];
  executableHaskellDepends = [
    attoparsec base binary bytestring containers direct-sqlite
    directory http-types process random text time unix wai warp
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
