let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/ac6b2166e7a9375683b8e98f860f273222337b16.tar.gz";
    sha256 = "0k6m5apwzg36qkm3wil1pf4q0lv1hp7r2imx4nfz9bfssnk9gj5w";
  }) { };
in
pkgs.mkShell {
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.duckdb pkgs.stdenv.cc.cc ];
  packages = [
    pkgs.haskell.compiler.ghc9141
    pkgs.cabal-install
    pkgs.duckdb
    pkgs.pkg-config
    pkgs.zlib
  ];
}
