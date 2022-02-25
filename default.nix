with import ./nixpkgs.nix {};

mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (p: with p; [
      # PyF
      adjunctions
      barbies
      bytestring
      cabal-install
      categories
      containers
      distributive
      either
      extra
      fin
      ghc-prim
      hedgehog
      linear
      semialign
      semigroupoids
      syb
      template-haskell
      text
      these
      transformers
      uniplate
      unliftio
      vec
      # yaya version 0.4.1.0 hasn't propagated to nixpkgs yet
    ]))
    ormolu # for Haskell formatting
  ];
}
