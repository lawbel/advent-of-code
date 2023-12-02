{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    systems = [ "x86_64-linux" ];
    imports = [ inputs.haskell-flake.flakeModule ];
    perSystem = { self', pkgs, config, ... }: {
      packages.default = self'.packages.adventofcode2022;
      haskellProjects.default = {
        basePackages = pkgs.haskell.packages.ghc902;
        packages.flow.source = "2.0.0.0";
        autoWire = [ "packages" "apps" "checks" ];
      };
      devShells.default = pkgs.mkShell {
        inputsFrom = [ config.haskellProjects.default.outputs.devShell ];
      };
    };
  };
}
