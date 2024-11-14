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

  ### This is a complicated flake. Here’s the rundown:
  ###
  ### overlays.default – includes all of the packages from cabal.project
  ### packages = {
  ###   default = points to `packages.${defaultGhcVersion}`
  ###   <ghcVersion>-<cabal-package> = an individual package compiled for one
  ###                                  GHC version
  ###   <ghcVersion>-all = all of the packages in cabal.project compiled for one
  ###                      GHC version
  ### };
  ### devShells = {
  ###   default = points to `devShells.${defaultGhcVersion}`
  ###   <ghcVersion> = a shell providing all of the dependencies for all
  ###                  packages in cabal.project compiled for one GHC version
  ### };
  ### checks.format = verify that code matches Ormolu expectations
  outputs = {
    concat,
    flake-utils,
    flaky,
    nixpkgs,
    self,
  }: let
    pname = "categorifier";

    supportedSystems = flaky.lib.defaultSystems;

    ## NB: `cabalProjectToNix` isn’t that smart, so we need to duplicate some of
    ##     its logic here. In particular, using `-Werror` and only supporting
    ##     `linear-base` on GHC 9 and later.
    ## TODO: This should filter out `linear-base` from GHC <9, but `cabalProject2nix` doesn’t make
    ##       that easy yet, so instead see the `projectConfigurations`, which simply disable the
    ##       corresponding builds on garnix.
    cabalPackages = pkgs: hpkgs:
      concat.lib.cabalProject2nix
      ./cabal.project
      pkgs
      hpkgs
      (old: {
        configureFlags = old.configureFlags ++ ["--ghc-options=-Werror"];
      });
  in
    {
      schemas = {
        inherit
          (flaky.schemas)
          overlays
          homeConfigurations
          packages
          devShells
          projectConfigurations
          checks
          formatter
          ;
      };

      overlays = {
        default =
          concat.lib.overlayHaskellPackages
          self.lib.supportedGhcVersions
          self.overlays.haskellWithDependencies;

        haskell = concat.lib.haskellOverlay cabalPackages;

        haskellWithDependencies = final: prev:
          nixpkgs.lib.composeManyExtensions [
            (hfinal: hprev: {
              #the included 0.3.1 no longer supports GHC 9.0
              "linear-base" = hprev.callHackageDirect {
                pkg = "linear-base";
                ver = "0.3.0";
                sha256 = "StvR4D8AwJUXhJE4PasvUq0N0oEQgl/FR4LbDUojBfE=";
              } {};
            })
            (concat.overlays.haskell final prev)
            ## TODO: I think this overlay is only needed by formatters, devShells, etc., so it
            ##       shouldn’t be included in the standard overlay.
            (flaky.overlays.haskell-dependencies final prev)
            (self.overlays.haskell final prev)
          ];
      };

      homeConfigurations =
        builtins.listToAttrs
        (builtins.map
          (flaky.lib.homeConfigurations.example
            pname
            self
            [
              ({pkgs, ...}: {
                home.packages = [
                  ## TODO: This should use `pkgs.haskellPackages`, but
                  ##       Categorifier doesn’t yet support GHC 9.4.
                  (pkgs.haskell.packages.${self.lib.defaultCompiler}.ghcWithPackages (hpkgs: [
                    hpkgs.categorifier-adjunctions-integration
                    hpkgs.categorifier-categories-integration
                    hpkgs.categorifier-category
                    hpkgs.categorifier-client
                    hpkgs.categorifier-concat-extensions-category
                    hpkgs.categorifier-concat-extensions-integration
                    hpkgs.categorifier-concat-integration
                    hpkgs.categorifier-fin-integration
                    hpkgs.categorifier-plugin
                    hpkgs.categorifier-unconcat-category
                    hpkgs.categorifier-unconcat-integration
                    hpkgs.categorifier-vec-integration
                  ]))
                ];
              })
            ])
          supportedSystems);

      lib = {
        ## TODO: Extract this automatically from `pkgs.haskellPackages`.
        defaultCompiler = "ghc928";

        ## Test the oldest revision possible for each minor release. If it’s not
        ## available in nixpkgs, test the oldest available, then try an older
        ## one via GitHub workflow. Additionally, check any revisions that have
        ## explicit conditionalization. And check whatever version `pkgs.ghc`
        ## maps to in the nixpkgs we depend on.
        testedGhcVersions = [
          self.lib.defaultCompiler
          # NB: need 8.10.7 specifically, because there's an API breakage that
          #     affects only it
          "ghc8107"
          # "ghcHEAD" # Not yet covered by dependency ranges
        ];

        ## The versions that are older than those supported by Nix that we
        ## prefer to test against.
        nonNixTestedGhcVersions = [
          "8.10.1"
          "9.0.1"
          "9.2.1"
          "9.2.2" # there's an API breakage introduced here
        ];

        ## However, provide packages in the default overlay for _every_
        ## supported version.
        supportedGhcVersions =
          self.lib.testedGhcVersions
          ++ [
            "ghc902"
            "ghc924"
            "ghc925"
            "ghc926"
            "ghc927"
          ];
      };
    }
    // flake-utils.lib.eachSystem supportedSystems (system: let
      pkgs = import nixpkgs {
        inherit system;
        # see these issues and discussions:
        # - https://github.com/NixOS/nixpkgs/issues/16394
        # - https://github.com/NixOS/nixpkgs/issues/25887
        # - https://github.com/NixOS/nixpkgs/issues/26561
        # - https://discourse.nixos.org/t/nix-haskell-development-2020/6170
        overlays = [self.overlays.default];
      };
    in {
      packages =
        {default = self.packages.${system}."${self.lib.defaultCompiler}_all";}
        // concat.lib.mkPackages pkgs self.lib.testedGhcVersions cabalPackages;

      devShells =
        ## TODO: determine why devShells are failing on i686.
        if system != "i686-linux"
        then
          {default = self.devShells.${system}.${self.lib.defaultCompiler};}
          // (
            concat.lib.mkDevShells
            pkgs
            self.lib.testedGhcVersions
            cabalPackages
            (hpkgs:
              [self.projectConfigurations.${system}.packages.path]
              ## NB: Haskell Language Server no longer supports GHC <9.
              ++ nixpkgs.lib.optional
              (nixpkgs.lib.versionAtLeast hpkgs.ghc.version "9")
              hpkgs.haskell-language-server)
          )
        else {};

      projectConfigurations =
        flaky.lib.projectConfigurations.default {inherit pkgs self;};

      checks = self.projectConfigurations.${system}.checks;
      formatter = self.projectConfigurations.${system}.formatter;
    });

  inputs = {
    # Currently contains our Haskell/Nix lib that should be extracted into its
    # own flake.
    concat = {
      inputs = {
        ## TODO: The version currently used by concat doesn’t support i686-linux.
        bash-strict-mode.follows = "flaky/bash-strict-mode";
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:compiling-to-categories/concat";
    };

    flake-utils.url = "github:numtide/flake-utils";

    flaky = {
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:sellout/flaky";
    };

    nixpkgs.url = "github:NixOS/nixpkgs/release-24.11";
  };
}
