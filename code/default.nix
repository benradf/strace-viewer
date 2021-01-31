let pkgs = import <nixpkgs> { };
in {
  code = pkgs.haskellPackages.callPackage ./code.nix { };
}
