{
  description = "categorifier";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-22.11";
    flake-utils.url = "github:numtide/flake-utils";
    concat = {
      url = "github:con-kitty/concat/wavewave-flake";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    linear-base = {
      url = "github:tweag/linear-base/v0.2.0";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, flake-utils, concat, linear-base }:
    flake-utils.lib.eachSystem flake-utils.lib.allSystems (system:
      let
        haskellLib = (import nixpkgs { inherit system; }).haskell.lib;
        overlay_deps = final: prev: {
          haskellPackages = prev.haskellPackages.override (old: {
            overrides =
              final.lib.composeExtensions (old.overrides or (_: _: { }))
              (self: super:
                { # test is broken for some GHC versions.
                  "PyF" = haskellLib.dontCheck super.PyF;
                  # test is broken.
                  "barbies" = haskellLib.dontCheck super.barbies;
                  # found the test is flaky.
                  "hls-pragmas-plugin" =
                    haskellLib.dontCheck super.hls-pragmas-plugin;
                  # linear-base 0.2.0
                  "linear-base" =
                    self.callCabal2nix "linear-base" linear-base { };
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
            value =
              let p = self.callCabal2nix name (./. + "/${path}") { };
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
                overlays = [ overlayGHC (concat.overlay.${system}) ]
                  ++ fullOverlays;
                inherit system;
                config.allowBroken = true;
              };

              individualPackages = builtins.listToAttrs (builtins.map
                ({ name, ... }: {
                  name = ghcVer + "_" + name;
                  value = builtins.getAttr name newPkgs.haskellPackages;
                }) categorifierPackages);

              allEnv = let
                hsenv = newPkgs.haskellPackages.ghcWithPackages (p:
                  let
                    deps = builtins.map ({ name, ... }: p.${name})
                      categorifierPackages;
                  in deps);
              in newPkgs.buildEnv {
                name = "all-packages";
                paths = [ hsenv ];
              };
            in individualPackages // { "${ghcVer}_all" = allEnv; };

        in { default = self.packages.${system}.ghc924_all; }
           // packagesOnGHC "ghc884"
           // packagesOnGHC "ghc8107"
           // packagesOnGHC "ghc902"
           // packagesOnGHC "ghc924"
           // packagesOnGHC "ghcHEAD";

        overlays = fullOverlays;

        devShells = let
          mkDevShell = ghcVer:
            let
              overlayGHC = final: prev: {
                haskellPackages = prev.haskell.packages.${ghcVer};
              };

              newPkgs = import nixpkgs {
                # Here we use the full overlays from this flake, but the categorifier-*
                # packages will not be provided in the shell. The overlay is only used
                # to extract dependencies.
                overlays = [ overlayGHC concat.overlay.${system} ]
                  ++ fullOverlays;
                inherit system;
                # linear-generics is broken upstream
                config.allowBroken = true;
              };

            in newPkgs.haskellPackages.shellFor {
              packages = ps:
                builtins.map (name: ps.${name}) categorifierPackageNames;
              buildInputs =
                # For these CLI tools, we use nixpkgs default
                [
                  newPkgs.haskell-language-server
                  newPkgs.haskell.packages.ghc8107.cabal-install
                  newPkgs.haskell.packages.ghc8107.hlint
                ];
              withHoogle = false;
            };
        in {
          default = self.devShells.${system}.ghc924;
          ghc884 = mkDevShell "ghc884";
          ghc8107 = mkDevShell "ghc8107";
          ghc902 = mkDevShell "ghc902";
          ghc924 = mkDevShell "ghc924";
          ghcHEAD = mkDevShell "ghcHEAD";
        };
      });
}
