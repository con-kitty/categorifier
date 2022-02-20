with import ./nixpkgs.nix {};

mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (p: with p; [
      cabal-install
    ]))
    ormolu # for Haskell formatting
  ];
}
