{
  description = "categorifier";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    concat = {
      url = "github:con-kitty/concat/wavewave-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, concat }:
    let
      pkgs = import nixpkgs {
        overlays = [ concat.overlay ];
        system = "x86_64-linux";
      };

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

    in {
      packages.x86_64-linux = let
        newHaskellPackages = pkgs.haskellPackages.override (old: {
          overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
            haskellOverlay;
        });

      in {
        inherit (newHaskellPackages)
          categorifier-category categorifier-client categorifier-common
          categorifier-duoids categorifier-ghc categorifier-hedgehog
          categorifier-adjunctions-integration
          categorifier-adjunctions-integration-test
          categorifier-categories-integration
          categorifier-categories-integration-test categorifier-concat-examples
          categorifier-concat-integration categorifier-concat-integration-test
          categorifier-concat-extensions-category
          categorifier-concat-extensions-integration
          categorifier-concat-extensions-integration-test
          categorifier-unconcat-category categorifier-unconcat-integration
          categorifier-unconcat-integration-test
          #categorifier-vec-integration categorifier-vec-integration-test
          categorifier-plugin categorifier-plugin-test categorifier-th;
      };

      # see these issues and discussions:
      # - https://github.com/NixOS/nixpkgs/issues/16394
      # - https://github.com/NixOS/nixpkgs/issues/25887
      # - https://github.com/NixOS/nixpkgs/issues/26561
      # - https://discourse.nixos.org/t/nix-haskell-development-2020/6170
      overlay = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
            haskellOverlay;
        });
      };

      devShell.x86_64-linux = let
        hsenv = pkgs.haskellPackages.ghcWithPackages
          (p: [ p.cabal-install p.concat-examples ]);
      in pkgs.mkShell { buildInputs = [ hsenv ]; };
    };
}
