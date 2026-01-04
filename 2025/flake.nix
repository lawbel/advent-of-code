{
  description = "Advent of Code 2025 (in Zig).";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    zig = {
      url = "github:mitchellh/zig-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    zls = {
      url = "github:zigtools/zls/0.15.1";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.zig-overlay.follows = "zig";
    };
  };

  outputs = {
    nixpkgs,
    zig,
    zls,
    ...
  }: let
    system = "x86_64-linux";
    name = "advent_2025";
    pkgs = import nixpkgs {inherit system;};
    lib = pkgs.lib;
    zig-exe = zig.packages.${system}."0.15.1";
    zls-exe = zls.packages.${system}.zls;

    max = 7;
    dayNums = builtins.genList (i: i + 1) max;
    dayStrs = builtins.map (lib.strings.fixedWidthNumber 2) dayNums;

    mkShellApp = sys: name: build: exports: {
      type = "app";
      program = "${mkShellProg sys name build exports}/bin/${name}";
    };
    mkShellProg = sys: name: build: exports:
      pkgs.writeShellApplication {
        inherit name;
        runtimeInputs = [zig-exe];
        text = ''
          ${exports}
          zig build ${build} \
            -Doptimize=ReleaseFast \
            -Dtarget=${sys} \
            --color off
        '';
      };

    export = num: "export ZIG_AOC_DAY_${num}=${./txt/day_${num}.txt}";
    rename = name: value: lib.attrsets.nameValuePair "day-${name}" value;

    dayApps = sys: lib.attrsets.mapAttrs' rename (days sys);
    days = sys: lib.attrsets.genAttrs dayStrs (day sys);
    day = sys: num: mkShellApp sys "day-${num}" "run-day-${num}" (export num);
  in {
    packages.${system}.default = pkgs.stdenvNoCC.mkDerivation {
      inherit name;
      src = ./.;
      nativeBuildInputs = [zig-exe];
      buildPhase = ''
        export ZIG_GLOBAL_CACHE_DIR=$TMPDIR/zig-cache
        zig build install \
          -Doptimize=ReleaseFast \
          -Dtarget=${system} \
          --color off \
          --prefix $out
      '';
    };

    apps.${system} =
      dayApps system
      // {
        all = mkShellApp system "all" "run-all" (
          lib.strings.concatMapStringsSep "\n" export dayStrs
        );
      };

    devShells.${system}.default = pkgs.mkShell {
      nativeBuildInputs = [zig-exe zls-exe];
    };
  };
}
