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
    yaya = {
      # 0.4.2.1
      url = "github:sellout/yaya/c96718e6de9787284ace3602451f194ac7711ace";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, flake-utils, concat, yaya }:
    flake-utils.lib.eachSystem flake-utils.lib.allSystems (system:
      let
        haskellLib = (import nixpkgs { inherit system; }).haskell.lib;
        overlay_deps = final: prev: {
          haskellPackages = prev.haskellPackages.override (old: {
            overrides =
              final.lib.composeExtensions (old.overrides or (_: _: { }))
              (self: super: {
                # test is broken.
                "barbies" = haskellLib.dontCheck super.barbies;
                # yaya 0.4.2.1
                "yaya" = self.callCabal2nix "yaya" (yaya + "/core") { };
                # yaya-unsafe 0.2.0.1
                "yaya-unsafe" =
                  self.callCabal2nix "yaya-unsafe" (yaya + "/unsafe") { };
                # test is broken for some GHC versions.
                "PyF" = haskellLib.dontCheck super.PyF;
              });
          });
        };

        parseCabalProject = import ./parse-cabal-project.nix;
        categorifierPackages = parseCabalProject ./cabal.project;
        categorifierPackageNames =
          builtins.map ({ name, ... }: name) categorifierPackages;
        haskellOverlay = self: super:
          builtins.listToAttrs (builtins.map ({ name, path }: {
            inherit name;
            value = let p = self.callCabal2nix name (./. + "/${path}") { };
            in haskellLib.appendConfigureFlag p "--ghc-options=-Werror";
          }) categorifierPackages);

        # see these issues and discussions:
        # - https://github.com/NixOS/nixpkgs/issues/16394
        # - https://github.com/NixOS/nixpkgs/issues/25887
        # - https://github.com/NixOS/nixpkgs/issues/26561
        # - https://discourse.nixos.org/t/nix-haskell-development-2020/6170
        fullOverlays = [
          overlay_deps
          (final: prev: {
            haskellPackages = prev.haskellPackages.override (old: {
              overrides =
                final.lib.composeExtensions (old.overrides or (_: _: { }))
                haskellOverlay;
            });
          })
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
                overlays =
                  [ overlayGHC overlay_deps (concat.overlay.${system}) ];
                inherit system;
                config.allowBroken = true;
              };

              newHaskellPackages = newPkgs.haskellPackages.override (old: {
                overrides =
                  newPkgs.lib.composeExtensions (old.overrides or (_: _: { }))
                  haskellOverlay;
              });

              individualPackages = builtins.listToAttrs (builtins.map
                ({ name, ... }: {
                  name = ghcVer + "_" + name;
                  value = builtins.getAttr name newHaskellPackages;
                }) categorifierPackages);

              allEnv = let
                # CI error: https://github.com/con-kitty/categorifier/runs/6457812761?check_suite_focus=true#step:5:8545
                excluded = [
                  "categorifier-concat-extensions-integration-test"
                  "categorifier-vec-integration-test"
                ];
                filtered = builtins.filter
                  ({ name, ... }: !(builtins.elem name excluded))
                  categorifierPackages;
                hsenv = newHaskellPackages.ghcWithPackages (p:
                  let deps = builtins.map ({ name, ... }: p.${name}) filtered;
                  in deps);
              in newPkgs.buildEnv {
                name = "all-packages";
                paths = [ hsenv ];
              };
            in individualPackages // { "${ghcVer}_all" = allEnv; };

        in packagesOnGHC "ghc8107" // packagesOnGHC "ghc884"
        // packagesOnGHC "ghc901" // packagesOnGHC "ghc921"
        // packagesOnGHC "ghcHEAD";

        overlays = fullOverlays;

        devShell = let
          ghcVer = "ghc8107";
          overlayGHC = final: prev: {
            haskellPackages = prev.haskell.packages.${ghcVer};
          };

          newPkgs = import nixpkgs {
            # Here we use the full overlays from this flake, but the categorifier-*
            # packages will not be provided in the shell. The overlay is only used
            # to extract dependencies.
            overlays = [ overlayGHC concat.overlay.${system} ] ++ fullOverlays;
            inherit system;
          };

        in newPkgs.haskellPackages.shellFor {
          packages = ps:
            builtins.map (name: ps.${name}) categorifierPackageNames;
          buildInputs = [
            newPkgs.haskellPackages.cabal-install
            newPkgs.haskell-language-server
          ];
          withHoogle = false;
        };
      });
}
