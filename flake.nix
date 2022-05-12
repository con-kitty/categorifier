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
        parseCabalProject = import ./parse-cabal-project.nix;
        excluded = [
          "categorifier-vec-integration"
          "categorifier-vec-integration-test"
        ];
        categorifierPackages = let parsed = parseCabalProject ./cabal.project;
        in builtins.filter ({ name, ... }: !(builtins.elem name excluded))
        parsed;
        categorifierPackageNames =
          builtins.map ({ name, ... }: name) categorifierPackages;
        haskellOverlay = self: super:
          builtins.listToAttrs (builtins.map ({ name, path }: {
            inherit name;
            value = self.callCabal2nix name (./. + "/${path}") { };
          }) categorifierPackages);

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
            in builtins.listToAttrs (builtins.map ({ name, ... }: {
              name = ghcVer + "_" + name;
              value = builtins.getAttr name newHaskellPackages;
            }) categorifierPackages);

        in builtins.trace (builtins.elemAt categorifierPackageNames 0)
        (packagesOnGHC "ghc8107" // packagesOnGHC "ghc884"
          // packagesOnGHC "ghc901" // packagesOnGHC "ghc921");

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
