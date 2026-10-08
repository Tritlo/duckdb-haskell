let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/ac6b2166e7a9375683b8e98f860f273222337b16.tar.gz";
    sha256 = "0k6m5apwzg36qkm3wil1pf4q0lv1hp7r2imx4nfz9bfssnk9gj5w";
  };
  pkgs = import nixpkgs { };
  clang = pkgs.llvmPackages;
  loaderPath = pkgs.lib.makeLibraryPath [ clang.libclang pkgs.duckdb pkgs.stdenv.cc.cc ];
  haskellPackages = pkgs.haskell.packages.ghc9141.override {
    overrides = self: super: {
      hs-bindgen = (self.callHackageDirect {
        pkg = "hs-bindgen";
        ver = "1.0.0.0";
        sha256 = "0k0mq38w9s1swvs30ibw5a11lc7szbp9dg7rr29jrqm8d7fb42dc";
      } { }).overrideAttrs (old: {
        nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ clang.clang clang.llvm pkgs.doxygen ];
        LD_LIBRARY_PATH = loaderPath;
      });
      hs-bindgen-runtime = self.callHackageDirect {
        pkg = "hs-bindgen-runtime";
        ver = "1.0.0.0";
        sha256 = "1ai7rls9afpr3dv0gf8p61g3k8bgi7cfl51nsckbhjlb3ap39vq4";
      } { };
      libclang-bindings = (pkgs.haskell.lib.doJailbreak (self.callHackageDirect {
        pkg = "libclang-bindings";
        ver = "0.2.0.0";
        sha256 = "1h0qixis80kbls005yxnrn5r38y90dwfzc9fg0rmgrmqlf2mxd84";
      } { })).overrideAttrs (old: {
        nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ clang.clang clang.llvm ];
        buildInputs = (old.buildInputs or [ ]) ++ [ clang.libclang ];
        configureFlags = (old.configureFlags or [ ]) ++ [
          "--configure-option=--with-so=${clang.libclang.lib}/lib/libclang.so"
        ];
        LD_LIBRARY_PATH = loaderPath;
      });
      c-expr-dsl = (self.callHackageDirect {
        pkg = "c-expr-dsl";
        ver = "0.2.0.0";
        sha256 = "1057glhmd0q73ipzxxa0myd58v65y0fl68v84l8kw65973w63dmp";
      } { }).overrideAttrs (old: {
        nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ clang.clang clang.llvm ];
        LD_LIBRARY_PATH = loaderPath;
      });
      c-expr-runtime = self.callHackageDirect {
        pkg = "c-expr-runtime";
        ver = "0.1.0.0";
        sha256 = "1d6d4ly07qw1k1wjxn191rcywzxjfrwlcfshs0gagxy5r8m7p2zx";
      } { };
      doxygen-parser = self.callHackageDirect {
        pkg = "doxygen-parser";
        ver = "0.1.2";
        sha256 = "1gk9am66qjrz8h3jclan92wzbqw9q8jq3k00m894dvwhyagx7a80";
      } { };
      debruijn = pkgs.haskell.lib.doJailbreak super.debruijn;
      dec = pkgs.haskell.lib.doJailbreak super.dec;
      # GHC 9.14 changes the Core that the fin inspection suite compares.
      fin = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak super.fin);
      # Run the Optics properties. Its Core inspection checks fail with GHC 9.14.
      optics = pkgs.haskell.lib.overrideCabal super.optics (old: {
        testFlags = (old.testFlags or [ ]) ++ [ "--pattern=properties" ];
      });
      skew-list = pkgs.haskell.lib.doJailbreak super.skew-list;
      vec = pkgs.haskell.lib.doJailbreak super.vec;
      duckdb-ffi = (self.callCabal2nix "duckdb-ffi" ../../duckdb-ffi { }).overrideAttrs (old: {
        nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ clang.clang clang.llvm pkgs.doxygen ];
        LD_LIBRARY_PATH = loaderPath;
        DUCKDB_TEST_VERSION = pkgs.duckdb.version;
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
assert pkgs.lib.versionAtLeast pkgs.duckdb.version "1.5.3";
assert pkgs.lib.versionOlder pkgs.duckdb.version "1.6";
{
  inherit (haskellPackages) duckdb-ffi duckdb-simple;
}
