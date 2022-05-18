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
    ghc-typelits-natnormalise = {
      # 0.7.7
      url =
        "github:clash-lang/ghc-typelits-natnormalise/a68b722a6b10a932621dbf578f1408745a37a5ca";
      flake = false;
    };
  };
  outputs =
    { self, nixpkgs, flake-utils, concat, yaya, ghc-typelits-natnormalise }:
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
                  # yaya 0.4.2.1
                  "yaya" = self.callCabal2nix "yaya" (yaya + "/core") { };
                  # yaya-unsafe 0.2.0.1
                  "yaya-unsafe" =
                    self.callCabal2nix "yaya-unsafe" (yaya + "/unsafe") { };
                } // (prev.lib.optionalAttrs
                  (prev.haskellPackages.ghc.version == "9.2.1") {
                    # loose base bound
                    "boring" = haskellLib.doJailbreak super.boring;
                    # fin-0.2.1
                    "fin" = haskellLib.doJailbreak super.fin;
                    # loosen ghc-bignum bound on GHC-9.2.1
                    "ghc-typelits-natnormalise" =
                      self.callCabal2nix "ghc-typelits-natnormalise"
                      ghc-typelits-natnormalise { };
                    # due to random, hashable on GHC-9.2.1
                    "linear" = haskellLib.doJailbreak super.linear_1_21_7;
                    # loosen base bound on GHC-9.2.1
                    "some" = haskellLib.doJailbreak super.some;
                    # loosen base bound on GHC-9.2.1
                    "universe-base" = super.universe-base_1_1_3;
                    # loosen base bound on GHC-9.2.1
                    "vec" = haskellLib.doJailbreak super.vec;
                  }));
          });
        };

        parseCabalProject = import ./parse-cabal-project.nix;
        categorifierPackages = parseCabalProject ./cabal.project;
        categorifierPackageNames =
          builtins.map ({ name, ... }: name) categorifierPackages;
        haskellOverlay = self: super:
          builtins.listToAttrs (builtins.map ({ name, path }: {
            inherit name;
            value = self.callCabal2nix name (./. + "/${path}") { };
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
                # CI error: https://github.com/con-kitty/categorifier/runs/6457812761?check_suite_focus=true#step:5:8545
                excluded = [
                  "categorifier-concat-extensions-integration-test"
                  "categorifier-vec-integration-test"
                ];
                filtered = builtins.filter
                  ({ name, ... }: !(builtins.elem name excluded))
                  categorifierPackages;
                hsenv = newPkgs.haskellPackages.ghcWithPackages (p:
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
              };

            in newPkgs.haskellPackages.shellFor {
              packages = ps:
                builtins.map (name: ps.${name}) categorifierPackageNames;
              buildInputs =
                # for the following tools, we use the nixpkgs default CLI tools.
                [
                  newPkgs.haskell.packages.ghc8107.cabal-install
                  newPkgs.haskell.packages.ghc8107.hlint
                ] ++
                # haskell-language-server on GHC 9.2.1 is broken yet.
                newPkgs.lib.optional (ghcVer != "ghc921")
                [ newPkgs.haskell-language-server ];
              withHoogle = false;
            };
        in {
          "ghc8107" = mkDevShell "ghc8107";
          "ghc921" = mkDevShell "ghc921";
        };
      });
}
