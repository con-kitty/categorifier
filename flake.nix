{
  description = "categorifier";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    flake-utils.url = "github:numtide/flake-utils";
    concat = {
      url = "github:con-kitty/concat/wavewave-flake";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utlis.follows = "flake-utils";
    };
  };
  outputs = { self, nixpkgs, flake-utils, concat }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        haskellOverlay = self: super: {
          "categorifier-category" =
            self.callCabal2nix "categorifier-category" ./category { };
          "categorifier-client" =
            self.callCabal2nix "categorifier-client" ./client { };
          "categorifier-common" =
            self.callCabal2nix "categorifier-common" ./common { };
          "categorifier-duoids" =
            self.callCabal2nix "categorifier-duoids" ./duoids { };
          "categorifier-ghc" = self.callCabal2nix "categorifier-ghc" ./ghc { };
          "categorifier-hedgehog" =
            self.callCabal2nix "categorifier-hedgehog" ./hedgehog { };
          "categorifier-adjunctions-integration" =
            self.callCabal2nix "categorifier-adjunctions-integration"
            ./integrations/adjunctions/integration { };
          "categorifier-adjunctions-integration-test" =
            self.callCabal2nix "categorifier-adjunctions-integration-test"
            ./integrations/adjunctions/integration-test { };
          "categorifier-categories-integration" =
            self.callCabal2nix "categorifier-categories-integration"
            ./integrations/categories/integration { };
          "categorifier-categories-integration-test" =
            self.callCabal2nix "categorifier-categories-integration-test"
            ./integrations/categories/integration-test { };
          "categorifier-concat-examples" =
            self.callCabal2nix "categorifier-concat-examples"
            ./integrations/concat/examples { };
          "categorifier-concat-integration" =
            self.callCabal2nix "categorifier-concat-integration"
            ./integrations/concat/integration { };
          "categorifier-concat-integration-test" =
            self.callCabal2nix "categorifier-concat-integration-test"
            ./integrations/concat/integration-test { };
          "categorifier-concat-extensions-category" =
            self.callCabal2nix "categorifier-concat-extensions-category"
            ./integrations/concat-extensions/category { };
          "categorifier-concat-extensions-integration" =
            self.callCabal2nix "categorifier-concat-extensions-integration"
            ./integrations/concat-extensions/integration { };
          "categorifier-concat-extensions-integration-test" =
            self.callCabal2nix "categorifier-concat-extensions-integration-test"
            ./integrations/concat-extensions/integration-test { };
          "categorifier-unconcat-category" =
            self.callCabal2nix "categorifier-unconcat-category"
            ./integrations/unconcat/category { };
          "categorifier-unconcat-integration" =
            self.callCabal2nix "categorifier-unconcat-integration"
            ./integrations/unconcat/integration { };
          "categorifier-unconcat-integration-test" =
            self.callCabal2nix "categorifier-unconcat-integration-test"
            ./integrations/unconcat/integration-test { };
          #"categorifier-vec-integration" =
          #  self.callCabal2nix "categorifier-vec-integration"
          #  ./integrations/vec/integration { };
          #"categorifier-vec-integration-test" =
          #  self.callCabal2nix "categorifier-vec-integration-test"
          #  ./integrations/vec/integration-test { };
          "categorifier-plugin" =
            self.callCabal2nix "categorifier-plugin" ./plugin { };
          "categorifier-plugin-test" =
            self.callCabal2nix "categorifier-plugin-test" ./plugin-test { };
          "categorifier-th" = self.callCabal2nix "categorifier-th" ./th { };

        };

        categorifierComponentNames = [
          "categorifier-category"
          "categorifier-client"
          "categorifier-common"
          "categorifier-duoids"
          "categorifier-ghc"
          "categorifier-hedgehog"
          "categorifier-adjunctions-integration"
          "categorifier-adjunctions-integration-test"
          "categorifier-categories-integration"
          "categorifier-categories-integration-test"
          "categorifier-concat-examples"
          "categorifier-concat-integration"
          "categorifier-concat-integration-test"
          "categorifier-concat-extensions-category"
          "categorifier-concat-extensions-integration"
          "categorifier-concat-extensions-integration-test"
          "categorifier-unconcat-category"
          "categorifier-unconcat-integration"
          "categorifier-unconcat-integration-test"
          # "categorifier-vec-integration"
          # "categorifier-vec-integration-test"
          "categorifier-plugin"
          "categorifier-plugin-test"
          "categorifier-th"
        ];

      in {
        # This package set is only useful for CI build test.
        # In practice, users will create a development environment composed by overlays.
        packages = let
          packagesOnGHC = ghcVer:
            let
              overlayGHC = final: prev: {
                haskellPackages = prev.haskell.packages.${ghcVer};
              };

              newPkgs = import nixpkgs {
                overlays = [ overlayGHC (concat.overlay.${system}) ];
                inherit system;
                config.allowBroken = true;
              };

              newHaskellPackages = newPkgs.haskellPackages.override (old: {
                overrides =
                  newPkgs.lib.composeExtensions (old.overrides or (_: _: { }))
                  haskellOverlay;
              });
            in builtins.listToAttrs (builtins.map (p: {
              name = ghcVer + "_" + p;
              value = builtins.getAttr p newHaskellPackages;
            }) categorifierComponentNames);
        in packagesOnGHC "ghc8107" // packagesOnGHC "ghc884"
        // packagesOnGHC "ghc901" // packagesOnGHC "ghc921";

        # see these issues and discussions:
        # - https://github.com/NixOS/nixpkgs/issues/16394
        # - https://github.com/NixOS/nixpkgs/issues/25887
        # - https://github.com/NixOS/nixpkgs/issues/26561
        # - https://discourse.nixos.org/t/nix-haskell-development-2020/6170
        overlay = final: prev: {
          haskellPackages = prev.haskellPackages.override (old: {
            overrides =
              final.lib.composeExtensions (old.overrides or (_: _: { }))
              haskellOverlay;
          });
        };

        devShell = let
          ghcVer = "ghc8107";
          overlayGHC = final: prev: {
            haskellPackages = prev.haskell.packages.${ghcVer};
          };

          newPkgs = import nixpkgs {
            overlays = [ overlayGHC concat.overlay.${system} ];
            inherit system;
          };

          hsenv = newPkgs.haskellPackages.ghcWithPackages
            (p: [ p.cabal-install p.concat-examples ]);
        in newPkgs.mkShell { buildInputs = [ hsenv ]; };
      });
}
