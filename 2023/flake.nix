{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nci = {
      url = "github:yusdacra/nix-cargo-integration";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ parts, nci, ... }:
    parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [ nci.flakeModule ./crates.nix ];
      perSystem = { pkgs, config, ... }:
        let
          crateOutputs = config.nci.outputs."advent-of-code-2023";
          devPkgs = [ pkgs.rust-analyzer ];
        in {
          devShells.default = crateOutputs.devShell.overrideAttrs (old: {
            packages = (old.packages or []) ++ devPkgs;
          });
          packages.default = crateOutputs.packages.release;
        };
    };
}
