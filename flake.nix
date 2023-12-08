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
      overlays = {
        default =
          concat.lib.overlayHaskellPackages
          self.lib.supportedGhcVersions
          self.overlays.haskell;

        haskell = concat.lib.haskellOverlay cabalPackages;
      };

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
          "ghc902"
          "ghc924"
          # "ghcHEAD" # Not yet covered by dependency ranges
        ];

        ## However, provide packages in the default overlay for _every_
        ## supported version.
        supportedGhcVersions =
          self.lib.testedGhcVersions
          ++ [
            "ghc925"
            "ghc926"
            "ghc927"
          ];
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
          self.lib.supportedGhcVersions
          overlay.haskellDependencies;

        # Concat has stopped supporting GHC 8, but we can re-add the overlays
        # here until it actually breaks something we depend on. We also add the
        # overlays for other supported versions that Concat doesn't yet provide
        # overlays for.
        missingConcat =
          concat.lib.overlayHaskellPackages
          # TODO: Use `nixpkgs.lib.subtractLists concat.lib.supportedGhcVersions
          #       self.lib.supportedGhcVersions` once Concat provides
          #       `supportedGhcVersions`.
          ["ghc8107" "ghc924" "ghc925" "ghc926" "ghc927"]
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
      packages =
        {default = self.packages.${system}."${self.lib.defaultCompiler}_all";}
        // concat.lib.mkPackages pkgs self.lib.testedGhcVersions cabalPackages;

      devShells =
        {default = self.devShells.${system}.${self.lib.defaultCompiler};}
        // concat.lib.mkDevShells
        pkgs
        self.lib.testedGhcVersions
        cabalPackages
        (hpkgs:
          [
            # For these CLI tools, we use nixpkgs default
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.hlint
          ]
          ## NB: Haskell Language Server no longer supports GHC <9.
          ++ nixpkgs.lib.optional
          (nixpkgs.lib.versionAtLeast hpkgs.ghc.version "9")
          hpkgs.haskell-language-server);

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
