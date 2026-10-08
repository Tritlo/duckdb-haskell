{ }:
let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/ac6b2166e7a9375683b8e98f860f273222337b16.tar.gz";
    sha256 = "0k6m5apwzg36qkm3wil1pf4q0lv1hp7r2imx4nfz9bfssnk9gj5w";
  };
  pkgs = import nixpkgs { };
  manifest = builtins.fromJSON (builtins.readFile ../../duckdb-ffi/vendor/duckdb-api.json);
  nativeDuckDB = pkgs.stdenv.mkDerivation {
    pname = "duckdb-preview";
    version = "2.0.0-dev0";
    src = pkgs.fetchurl {
      name = "duckdb-${manifest.commit}.tar.gz";
      inherit (manifest.source_archive) url sha256;
    };
    nativeBuildInputs = [ pkgs.cmake pkgs.python3 ];
    cmakeFlags = [
      "-DBUILD_UNITTESTS=OFF"
      "-DBUILD_SHELL=OFF"
      "-DBUILD_EXTENSIONS=icu;json;parquet"
      "-DDUCKDB_EXPLICIT_VERSION=v2.0.0-dev0"
      "-DGIT_COMMIT_HASH=${manifest.commit}"
    ];
    buildTarget = "duckdb";
    installPhase = ''
      runHook preInstall
      mkdir -p $out/lib $out/include $out/share/licenses/duckdb
      cp src/libduckdb${pkgs.stdenv.hostPlatform.extensions.sharedLibrary} $out/lib/
      cp ../src/include/duckdb*.h $out/include/
      cp ../LICENSE $out/share/licenses/duckdb/LICENSE
      runHook postInstall
    '';
  };
  haskellPackages = pkgs.haskell.packages.ghc9141.override {
    overrides = self: _super: {
      duckdb-ffi = (self.callCabal2nix "duckdb-ffi" ../../duckdb-ffi { duckdb = nativeDuckDB; }).overrideAttrs (old: {
        DUCKDB_TEST_VERSION = nativeDuckDB.version;
      });
      geometry-simple = self.callHackageDirect {
        pkg = "geometry-simple";
        ver = "0.1.1.0";
        sha256 = "1nicg1v3v76x8wp02a38xpqzyl88dcn6m1gbn3ry3my2jj94gccw";
      } { };
      duckdb-simple = self.callCabal2nix "duckdb-simple" ../../duckdb-simple { };
    };
  };
in
{
  inherit (haskellPackages) duckdb-ffi duckdb-simple;
}
