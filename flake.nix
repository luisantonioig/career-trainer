{
  description = "Haskell web application with a Nix-managed development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        hsPkgs = pkgs.haskell.packages.ghc912.override {
          overrides = hself: hsuper: {
            scotty = hself.callHackageDirect {
              pkg = "scotty";
              ver = "0.30";
              sha256 = "0z9z2k13kd63hgvjd711wbb14kwkcipaqaxv1721xaz9nzmra4jj";
            } { };
            lucid2 = hself.callHackageDirect {
              pkg = "lucid2";
              ver = "0.0.20260427";
              sha256 = "1hqilw87z1wwbp7z9kyzy0zlc233gml6x9y22c35b67vfkb6vf2f";
            } { };
          };
        };
        app = hsPkgs.callCabal2nix "career-trainer" ./. { };
      in
      {
        packages.default = app;

        apps.default = {
          type = "app";
          program = "${app}/bin/career-trainer";
          meta.description = "AI-assisted career training web application";
        };

        devShells.default = hsPkgs.shellFor {
          packages = _: [ app ];

          nativeBuildInputs = [
            hsPkgs.cabal-install
            hsPkgs.ghcid
            pkgs.haskell-language-server
            pkgs.fourmolu
          ];

          shellHook = ''
            echo "Haskell dev shell ready"
            echo "Run: cabal run career-trainer"
          '';
        };

        formatter = pkgs.nixfmt;
      }
    );
}
