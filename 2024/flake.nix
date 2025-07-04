{
  description = "Advent of Code 2024 (in Zig).";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    zig.url = "github:cloudef/zig2nix";
    zls.url = "github:zigtools/zls";
  };

  outputs = { self, nixpkgs, zig, zls, ... }:
    let
      system = "x86_64-linux";
      name = "advent_2024";
      pkgs = import nixpkgs { inherit system; };
      zig-exe = zig.packages.${system}."zig-0_14_0";
      zls-exe = zls.packages.${system}.zls;

      inherit (pkgs.lib) attrsets lists strings;
      dayNums = lists.range 1 7;
      dayStrs = map (strings.fixedWidthNumber 2) dayNums;
      rename = name: value: attrsets.nameValuePair "day-${name}" value;
      daysNumbered = target: attrsets.genAttrs dayStrs (runDay target);
      daysNamed = target: attrsets.mapAttrs' rename (daysNumbered target);
      runDay = target: num: pkgs.writeShellApplication {
        name = "day-${num}";
        runtimeInputs = [ zig-exe ];
        text = ''
          export ZIG_AOC_DAY_${num}=${./txt/day_${num}.txt}
          zig build day-${num} \
            -Doptimize=ReleaseSmall \
            -Dtarget=${target} \
            --color off
        '';
      };

    in {
      packages.${system} = daysNamed system // {
        default = self.packages.${system}.compile;
        compile = pkgs.stdenvNoCC.mkDerivation {
          inherit name;
          src = ./.;
          nativeBuildInputs = [ zig-exe ];
          buildPhase = ''
            export ZIG_GLOBAL_CACHE_DIR=$TMPDIR/zig-cache
            zig build install \
              -Doptimize=ReleaseFast \
              -Dtarget=${system} \
              --color off \
              --prefix $out
          '';
        };
      };

      devShells.${system}.default = pkgs.mkShell {
        nativeBuildInputs = [
          zig-exe
          zls-exe
        ];
      };
    };
}
