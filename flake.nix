{
  description = "Defining novel interpretations of Haskell programs";

  nixConfig = {
    ## https://github.com/NixOS/rfcs/blob/master/rfcs/0045-deprecate-url-syntax.md
    extra-experimental-features = ["no-url-literals"];
    extra-substituters = ["https://cache.garnix.io"];
    extra-trusted-public-keys = [
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    ];
    ## Isolate the build.
    registries = false;
    sandbox = "relaxed";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    concat,
  }: let
    parseCabalProject = import ./parse-cabal-project.nix;
    categorifierPackages = parseCabalProject ./cabal.project;
    categorifierPackageNames =
      builtins.map ({name, ...}: name) categorifierPackages;
  in
    {
      overlays = {
        default =
          concat.lib.overlayHaskellPackages
          ["ghc8107" "ghc902" "ghc928" "ghcHEAD"]
          self.overlays.haskell;

        haskell = final: prev: self: super:
          builtins.listToAttrs (builtins.map ({
              name,
              path,
            }: {
              inherit name;
              value = let
                p = self.callCabal2nix name (./. + "/${path}") {};
              in
                final.haskell.lib.appendConfigureFlag p "--ghc-options=-Werror";
            })
            categorifierPackages);
      };
    }
    // flake-utils.lib.eachSystem flake-utils.lib.allSystems (system: let
      overlay = {
        haskellDependencies = final: prev: self: super: {
          #the included 0.3.1 no longer supports GHC 9.0
          "linear-base" = self.callHackageDirect {
            pkg = "linear-base";
            ver = "0.3.0";
            sha256 = "StvR4D8AwJUXhJE4PasvUq0N0oEQgl/FR4LbDUojBfE=";
          } {};
        };

        dependencies =
          concat.lib.overlayHaskellPackages
          ["ghc8107" "ghc902" "ghc928" "ghcHEAD"]
          overlay.haskellDependencies;

        # Concat has stopped supporting GHC 8, but we can re-add the overlays
        # here until it actually breaks something we depend on.
        missingConcat =
          concat.lib.overlayHaskellPackages
          ["ghc8107" "ghcHEAD"]
          concat.overlays.haskell;
      };

      pkgs = import nixpkgs {
        overlays = [
          concat.overlays.default
          overlay.dependencies
          overlay.missingConcat
          # see these issues and discussions:
          # - https://github.com/NixOS/nixpkgs/issues/16394
          # - https://github.com/NixOS/nixpkgs/issues/25887
          # - https://github.com/NixOS/nixpkgs/issues/26561
          # - https://discourse.nixos.org/t/nix-haskell-development-2020/6170
          self.overlays.default
        ];
        inherit system;
      };
    in {
      # This package set is only useful for CI build test.
      # In practice, users will create a development environment composed by overlays.
      packages = let
        packagesOnGHC = ghcVer: let
          hpkgs = pkgs.haskell.packages.${ghcVer};

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
            pkgs.buildEnv {
              name = "all-packages";
              paths = [hsenv];
            };
        in
          individualPackages // {"${ghcVer}_all" = allEnv;};
      in
        {default = self.packages.${system}.ghc928_all;}
        # need 8.10.7 specifically, because there's an API breakage that affects only it
        // packagesOnGHC "ghc8107"
        // packagesOnGHC "ghc902"
        // packagesOnGHC "ghc928"
        // packagesOnGHC "ghcHEAD";

      devShells = let
        mkDevShell = ghcVer: let
          hpkgs = pkgs.haskell.packages.${ghcVer};
        in
          hpkgs.shellFor {
            packages = ps:
              builtins.map (name: ps.${name}) categorifierPackageNames;
            buildInputs = [
              # For these CLI tools, we use nixpkgs default
              pkgs.haskellPackages.cabal-install
              pkgs.haskellPackages.hlint
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
        "ghcHEAD" = mkDevShell "ghcHEAD";
      };

      formatter = pkgs.alejandra;
    });

  inputs = {
    concat = {
      url = "github:compiling-to-categories/concat";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
  };
}
