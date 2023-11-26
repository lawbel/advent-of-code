{...}: {
  perSystem = { pkgs, config, ... }:
    let
      crateName = "advent-of-code-2023";
    in {
      nci.projects.${crateName}.path = ./.;
      nci.crates.${crateName} = {};
    };
}
