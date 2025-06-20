{
  description = "Advent of Code 2024 (in Zig).";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    zig.url = "github:mitchellh/zig-overlay";
    zls.url = "github:zigtools/zls";
  };

  outputs = { self, nixpkgs, zig, zls, ... }:
    let
      system = "x86_64-linux";
      name = "advent_2024";
      pkgs = import nixpkgs { inherit system; };
      zig-exe = zig.packages.${system}."0.14.0";
      zls-exe = zls.packages.${system}.zls;

    in {
      packages.${system}.default = pkgs.stdenvNoCC.mkDerivation {
        inherit name;
        src = ./.;
        nativeBuildInputs = [ zig-exe ];
        buildPhase = ''
          zig build install \
            --global-cache-dir .cache \
            --prefix $out
        '';
        doCheck = false;
        checkPhase = ''
          zig build test --global-cache-dir .cache
        '';
      };

      devShells.${system}.default = pkgs.mkShell {
        nativeBuildInputs = [
          zig-exe
          zls-exe
        ];
      };
    };
}
