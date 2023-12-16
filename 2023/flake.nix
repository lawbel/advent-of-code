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
    let
      crateName = "advent-of-code-2023";
      crates = { ... }: {
        perSystem = { pkgs, config, ... }: {
          nci.projects.${crateName}.path = ./.;
          nci.crates.${crateName} = {};
        };
      };
    in
      parts.lib.mkFlake { inherit inputs; } {
        systems = [ "x86_64-linux" ];
        imports = [ nci.flakeModule crates ];
        perSystem = { pkgs, config, ... }:
          let
            crateOutputs = config.nci.outputs.${crateName};
            devPkgs = [ pkgs.rust-analyzer ];
          in {
            devShells.default = crateOutputs.devShell.overrideAttrs (old: {
              packages = (old.packages or []) ++ devPkgs;
            });
            packages.default = crateOutputs.packages.release;
          };
      };
}
