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
        "categorifier-plugin" =
          self.callCabal2nix "categorifier-plugin" ./plugin { };
        "categorifier-th" = self.callCabal2nix "categorifier-th" ./th { };
      };

      newHaskellPackages =
        pkgs.haskellPackages.override { overrides = haskellOverlay; };

    in {
      packages.x86_64-linux = {
        inherit (newHaskellPackages)
          categorifier-category categorifier-client categorifier-common
          categorifier-duoids categorifier-ghc categorifier-plugin
          categorifier-th;
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
        hsenv = pkgs.haskellPackages.ghcWithPackages (p: [ p.cabal-install ]);
      in pkgs.mkShell { buildInputs = [ hsenv ]; };
    };
}
