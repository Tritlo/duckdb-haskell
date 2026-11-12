{ }:
let
  pins = import ./pins.nix;
  pkgs = pins.pkgs;
  manifest = builtins.fromJSON (builtins.readFile ../../duckdb-ffi/vendor/duckdb-api.json);
  nativeDuckDB = pkgs.stdenv.mkDerivation {
    pname = "duckdb";
    version = pkgs.lib.removePrefix "v" manifest.ref;
    src = pkgs.fetchurl {
      name = "duckdb-${manifest.commit}.tar.gz";
      inherit (manifest.source_archive) url sha256;
    };
    nativeBuildInputs = [ pkgs.cmake pkgs.python3 ];
    cmakeFlags = [
      "-DBUILD_UNITTESTS=OFF"
      "-DBUILD_SHELL=OFF"
      "-DBUILD_EXTENSIONS=icu;json;parquet"
      "-DDUCKDB_EXPLICIT_VERSION=${manifest.ref}"
      "-DGIT_COMMIT_HASH=${manifest.commit}"
    ];
    buildTarget = "duckdb";
    installPhase = ''
      runHook preInstall
      mkdir -p $out/lib $out/include $out/share/licenses/duckdb
      cp -P src/libduckdb*${pkgs.stdenv.hostPlatform.extensions.sharedLibrary}* $out/lib/
      cp ../src/include/duckdb*.h $out/include/
      cp ../LICENSE $out/share/licenses/duckdb/LICENSE
      runHook postInstall
    '';
  };
  haskellPackages = pkgs.haskell.packages.ghc9141.override {
    overrides = self: _super: {
      hs-bindgen-runtime = pins.hsBindgenRuntime self;
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
  inherit nativeDuckDB;
  inherit (haskellPackages) duckdb-ffi duckdb-simple;
}
