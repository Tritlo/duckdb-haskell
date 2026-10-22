# Pins shared by the development, generation, and CI environments.
{
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/ac6b2166e7a9375683b8e98f860f273222337b16.tar.gz";
    sha256 = "0k6m5apwzg36qkm3wil1pf4q0lv1hp7r2imx4nfz9bfssnk9gj5w";
  }) { };
  # The generated bindings and the packages that compile them use this runtime.
  hsBindgenRuntime = haskellPackages: haskellPackages.callHackageDirect {
    pkg = "hs-bindgen-runtime";
    ver = "1.0.0.0";
    sha256 = "1ai7rls9afpr3dv0gf8p61g3k8bgi7cfl51nsckbhjlb3ap39vq4";
  } { };
}
