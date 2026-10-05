let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/ac6b2166e7a9375683b8e98f860f273222337b16.tar.gz";
    sha256 = "0k6m5apwzg36qkm3wil1pf4q0lv1hp7r2imx4nfz9bfssnk9gj5w";
  };
  pkgs = import nixpkgs { };
  haskellPackages = pkgs.haskell.packages.ghc9141.override {
    overrides = self: _super: {
      duckdb-ffi = (self.callCabal2nix "duckdb-ffi" ../../duckdb-ffi { }).overrideAttrs (_: {
        DUCKDB_TEST_VERSION = pkgs.duckdb.version;
      });
      duckdb-simple = self.callCabal2nix "duckdb-simple" ../../duckdb-simple { };
    };
  };
in
assert pkgs.lib.versionAtLeast pkgs.duckdb.version "1.5.3";
assert pkgs.lib.versionOlder pkgs.duckdb.version "1.6";
{
  inherit (haskellPackages) duckdb-ffi duckdb-simple;
}
