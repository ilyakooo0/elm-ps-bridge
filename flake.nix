{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-linux"
      "aarch64-darwin"
    ]
      (system:
        let
          pkgs = import nixpkgs {
            overlays = [ haskellNix.overlay ];
            inherit system;
            inherit (haskellNix) config;
          };

          project = pkgs.haskell-nix.project' {
            src = ./.;
          };
          flake = project.flake { };
        in
        flake //
        {
          apps.default = {
            type = "app";
            program = "${flake.packages."elm-ps-bridge:exe:elm-ps-bridge"}/bin/elm-ps-bridge";
          };
        }
      );
}
