{config, flaky, lib, pkgs, self, ...}: {
  project = {
    name = "categorifier";
    summary = "Defining novel interpretations of Haskell programs";
  };

  imports = [
    ./github-ci.nix
    ./hlint.nix
  ];

  ## dependency management
  services.renovate.enable = true;

  ## development
  programs = {
    direnv.enable = true;
    # This should default by whether there is a .git file/dir (and whether it’s
    # a file (worktree) or dir determines other things – like where hooks
    # are installed.
    git = {
      enable = true;
      ignores = [
        # Cabal build
        "dist-newstyle"
      ];
    };
  };

  ## formatting
  editorconfig.enable = true;
  project.file.".dir-locals.el".source = lib.mkForce ../emacs/.dir-locals.el;
  programs = {
    treefmt = {
      enable = true;
      ## Haskell formatter
      ## TODO: `validity`, required by Ormolu, fails to build on i686-linux.
      programs.ormolu.enable = pkgs.system != "i686-linux";
    };
    vale = {
      enable = true;
      excludes = [
        "*.cabal"
        "*.hs"
        "*.lhs"
        "./cabal.project"
      ];
      vocab.${config.project.name}.accept = [
        "bugfix"
        "Categorifier"
        "categorified"
        "categorify"
        "comonad"
        "Conal"
        "concat"
        "conditionalize"
        "functor"
        "GADT"
        "GHC"
        "Hask"
        "inline"
        "inlining"
        "invasive"
        "Kleisli"
        "Kmett"
        "[Mm]onoidal"
        "monomorphization"
        "pragma"
        "unfolding"
      ];
    };
  };

  ## CI
  services.garnix = {
    enable = true;
    builds.exclude = [
      # TODO: Remove once garnix-io/issues#17 is fixed.
      "devShells.aarch64-darwin.ghc928"
      # TODO: Remove once garnix-io/garnix#285 is fixed.
      "homeConfigurations.x86_64-darwin-${config.project.name}-example"
      # TODO: Remove these and filter the corresponding packages from the flake (see the comments on
      #       `cabalPackages` for more information.
      "devShells.*.ghc8107"
      "packages.*.ghc8107_all"
      "packages.*.ghc8107_categorifier-ghc-bignum-integration"
      "packages.*.ghc8107_categorifier-ghc-bignum-integration-test"
      "packages.*.ghc8107_categorifier-linear-base-integration"
      "packages.*.ghc8107_categorifier-linear-base-integration-test"
    ];
  };
  ## FIXME: Shouldn’t need `mkForce` here (or to duplicate the base contexts).
  ##        Need to improve module merging.
  services.github.settings.branches.main.protection.required_status_checks.contexts =
    lib.mkForce
      (map (ghc: "CI / build (${ghc}) (pull_request)") self.lib.nonNixTestedGhcVersions
      ++ lib.concatMap flaky.lib.garnixChecks (
        lib.concatMap (ghc: [
          (sys: "devShell ghc${ghc} [${sys}]")
          (sys: "package ghc${sys}_all [${sys}]")
        ])
        self.lib.testedGhcVersions
        ++ [
          (sys: "homeConfig ${sys}-${config.project.name}-example")
          (sys: "package default [${sys}]")
          ## FIXME: These are duplicated from the base config
          (sys: "check formatter [${sys}]")
          (sys: "devShell default [${sys}]")
        ]));

  ## publishing
  services.flakehub.enable = true;
  services.github.enable = true;
  services.github.settings.repository.topics = ["category-theory" "plugin"];
}
