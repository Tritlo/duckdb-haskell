let
  pkgs = (import ./pins.nix).pkgs;
  nativeDuckDB = (import ./ci.nix { }).nativeDuckDB;
in
pkgs.mkShell {
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ nativeDuckDB pkgs.stdenv.cc.cc ];
  packages = [
    pkgs.haskell.compiler.ghc9141
    pkgs.cabal-install
    nativeDuckDB
    pkgs.pkg-config
    pkgs.zlib
  ];
}
