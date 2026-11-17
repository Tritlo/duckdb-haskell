let
  pkgs = (import ./pins.nix).pkgs;
  source = pkgs.runCommand "haddock-ghc-9.14.1-source" {
    # The compiler source derivation retains its original archive pin.
    src = pkgs.haskell.compiler.ghc9141.src.src;
    nativeBuildInputs = [ pkgs.xz pkgs.patch ];
  } ''
    mkdir -p "$out"
    tar -xJf "$src" --strip-components=3 -C "$out" ghc-9.14.1/utils/haddock
    patch -d "$out/haddock-api" -p1 < ${../haddock/instance-permalinks.patch}
  '';
  # Build release tools without their tests, documentation, or profiling libraries.
  releaseTool = package: pkgs.haskell.lib.overrideCabal package (_: {
    doCheck = false;
    doHaddock = false;
    enableLibraryProfiling = false;
  });
  haskellPackages = pkgs.haskell.packages.ghc9141.override {
    overrides = self: _super: {
      haddock-library = releaseTool (self.callCabal2nix "haddock-library" "${source}/haddock-library" { });
      haddock-api = releaseTool (self.callCabal2nix "haddock-api" "${source}/haddock-api" { });
      haddock = releaseTool (self.callCabal2nix "haddock" source { });
    };
  };
in
haskellPackages.haddock
