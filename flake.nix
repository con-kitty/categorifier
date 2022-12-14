{
  description = "categorifier";

  outputs = { self, nixpkgs, flake-utils, concat, linear-base }: let
    supportedGhcVersions = ["ghc884" "ghc8107" "ghc902" "ghc924" "ghcHEAD"];
    dependencies =
      nixpkgs.lib.composeExtensions
        concat.overlays.default
        (final:
          concat.lib.overlayHaskellPackages
            supportedGhcVersions
            (ghcVer: hfinal: hprev: let
              haskellLib = final.haskell.lib;
            in {
              # test is broken for some GHC versions.
              "PyF" = haskellLib.dontCheck hprev.PyF;
              # test is broken.
              "barbies" = haskellLib.dontCheck hprev.barbies;
              # found the test is flaky.
              "hls-pragmas-plugin" =
                haskellLib.dontCheck hprev.hls-pragmas-plugin;
              # linear-base 0.2.0
              "linear-base" =
                hfinal.callCabal2nix "linear-base" linear-base { };
            })
            final);
    cabalPackages = concat.lib.cabalProject2nix ./cabal.project;
  in {
    overlays.default =
      nixpkgs.lib.composeExtensions
        dependencies
        (final:
          concat.lib.overlayHaskellPackages
            supportedGhcVersions
            (ghcVer: hfinal: hprev: cabalPackages final ghcVer)
            final);
  }
  // flake-utils.lib.eachSystem flake-utils.lib.allSystems (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        # NB: This uses `self.overlays.default` instead of `dependencies`
        #     because packages need to be able to find other packages in this
        #     flake as dependencies.
        overlays = [self.overlays.default];
      };
    in {
      # This package set is only useful for CI build test.
      # In practice, users will create a development environment composed by overlays.
      packages = 
        { default = self.packages.${system}.ghc902_all; }
        // concat.lib.mkPackages pkgs supportedGhcVersions cabalPackages;
      
      devShells =
        { default = self.devShells.${system}.ghc902; }
        // concat.lib.mkDevShells pkgs supportedGhcVersions cabalPackages;
    });
  
  inputs = {
    concat = {
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
      url = github:con-kitty/concat/updated-flake;
    };
    
    flake-utils.url = github:numtide/flake-utils;
    
    linear-base = {
      flake = false;
      url = github:tweag/linear-base/v0.2.0;
    };
    
    nixpkgs.url = github:NixOS/nixpkgs/release-22.11;
  };
}
