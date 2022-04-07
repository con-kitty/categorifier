with import ./nixpkgs.nix {};

mkShell {
  buildInputs = [
    (haskell.packages.ghc922.ghcWithPackages (p: with p; [
      cabal-install
    ]))
    ormolu # for Haskell formatting
  ];
}
