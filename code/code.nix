{ mkDerivation, attoparsec, base, binary, bytestring, containers
, direct-sqlite, directory, file-embed, hinotify, http-client
, http-types, lib, lucid, lucid-svg, mtl, process, QuickCheck
, random, tasty, tasty-hunit, tasty-quickcheck, temporary, text
, time, transformers, unix, wai, warp
}:
mkDerivation {
  pname = "code";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring containers direct-sqlite directory
    http-client http-types process text time transformers unix wai warp
  ];
  executableHaskellDepends = [
    attoparsec base binary bytestring containers direct-sqlite
    directory file-embed hinotify http-client http-types lucid
    lucid-svg mtl process QuickCheck random tasty tasty-hunit
    tasty-quickcheck temporary text time transformers unix wai warp
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
