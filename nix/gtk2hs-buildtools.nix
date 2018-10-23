{ mkDerivation, alex, array, base, Cabal, containers, directory
, filepath, happy, hashtables, pretty, process, random, stdenv
}:
mkDerivation {
  pname = "gtk2hs-buildtools";
  version = "0.13.4.0";
  sha256 = "0f3e6ba90839efd43efe8cecbddb6478a55e2ce7788c57a0add4df477dede679";
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    array base Cabal containers directory filepath hashtables pretty
    process random
  ];
  libraryToolDepends = [ alex happy ];
  executableHaskellDepends = [ base ];
  homepage = "http://projects.haskell.org/gtk2hs/";
  description = "Tools to build the Gtk2Hs suite of User Interface libraries";
  license = stdenv.lib.licenses.gpl2;
}
