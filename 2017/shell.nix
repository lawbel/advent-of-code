let
  pkgs = import <nixpkgs> {};
in
  pkgs.haskellPackages.developPackage {
    root = ./.;
    name = "adventofcode2017";
    modifier = deriv: pkgs.haskell.lib.addBuildTools deriv [
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.stack
      pkgs.haskellPackages.haskell-language-server
    ];
  }
