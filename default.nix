with import ./nixpkgs.nix {};

mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (p: with p; [
      cabal-install
    ]))
    graphviz-nox # for concat-examples tests
  ];
}
