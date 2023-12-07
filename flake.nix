{
  description = "categorifier";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
    flake-utils.url = "github:numtide/flake-utils";
    concat = {
      url = "github:compiling-to-categories/concat";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    concat,
  }:
    flake-utils.lib.eachSystem flake-utils.lib.allSystems (system: let
      haskellLib = (import nixpkgs {inherit system;}).haskell.lib;
      overlay_deps = self: super: {
        #the included 0.3.1 no longer supports GHC 9.0
        "linear-base" = self.callHackageDirect {
          pkg = "linear-base";
          ver = "0.3.0";
          sha256 = "StvR4D8AwJUXhJE4PasvUq0N0oEQgl/FR4LbDUojBfE=";
        } {};
      };

      # Concat has stopped supporting GHC 8, but we can re-add the overlays
      # here until it actually breaks something we depend on.
      missingConcatOverlay =
        concat.lib.overlayHaskellPackages
        ["ghc8107" "ghcHEAD"]
        concat.overlays.haskell;

      parseCabalProject = import ./parse-cabal-project.nix;
      categorifierPackages = parseCabalProject ./cabal.project;
      categorifierPackageNames =
        builtins.map ({name, ...}: name) categorifierPackages;
      haskellOverlay = self: super:
        builtins.listToAttrs (builtins.map ({
            name,
            path,
          }: {
            inherit name;
            value = let
              p = self.callCabal2nix name (./. + "/${path}") {};
            in
              haskellLib.appendConfigureFlag p "--ghc-options=-Werror";
          })
          categorifierPackages);

      # see these issues and discussions:
      # - https://github.com/NixOS/nixpkgs/issues/16394
      # - https://github.com/NixOS/nixpkgs/issues/25887
      # - https://github.com/NixOS/nixpkgs/issues/26561
      # - https://discourse.nixos.org/t/nix-haskell-development-2020/6170
      fullOverlays =
        concat.lib.overlayHaskellPackages
        ["ghc8107" "ghc902" "ghc928" "ghcHEAD"]
        (final: prev: nixpkgs.lib.composeExtensions overlay_deps haskellOverlay);

      newPkgs = import nixpkgs {
        overlays = [
          concat.overlays.default
          missingConcatOverlay
          fullOverlays
        ];
        inherit system;
      };
    in {
      # This package set is only useful for CI build test.
      # In practice, users will create a development environment composed by overlays.
      packages = let
        packagesOnGHC = ghcVer: let
          hpkgs = newPkgs.haskell.packages.${ghcVer};

          individualPackages = builtins.listToAttrs (builtins.map
            ({name, ...}: {
              name = ghcVer + "_" + name;
              value = builtins.getAttr name hpkgs;
            })
            categorifierPackages);

          allEnv = let
            hsenv =
              hpkgs.ghcWithPackages (p:
                map (name: p.${name}) categorifierPackageNames);
          in
            newPkgs.buildEnv {
              name = "all-packages";
              paths = [hsenv];
            };
        in
          individualPackages // {"${ghcVer}_all" = allEnv;};
      in
        packagesOnGHC "ghc8107"
        // packagesOnGHC "ghc902"
        // packagesOnGHC "ghc928"
        // packagesOnGHC "ghcHEAD";

      overlays.default = fullOverlays;

      devShells = let
        mkDevShell = ghcVer: let
          hpkgs = newPkgs.haskell.packages.${ghcVer};
        in
          hpkgs.shellFor {
            packages = ps:
              builtins.map (name: ps.${name}) categorifierPackageNames;
            buildInputs = [
              # For these CLI tools, we use nixpkgs default
              newPkgs.haskellPackages.cabal-install
              newPkgs.haskellPackages.hlint
              # But this one has to match our GHC
              hpkgs.haskell-language-server
            ];
            withHoogle = false;
          };
      in {
        "default" = mkDevShell "ghc928";
        "ghc8107" = mkDevShell "ghc8107";
        "ghc902" = mkDevShell "ghc902";
        "ghc928" = mkDevShell "ghc928";
      };

      formatter = newPkgs.alejandra;
    });
}
