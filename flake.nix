{
  description = "categorifier";
  inputs = { nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11"; };
  outputs = { self, nixpkgs }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux;

    in {
      devShell.x86_64-linux = let
        hsenv = pkgs.haskellPackages.ghcWithPackages (p: [ p.cabal-install ]);
      in pkgs.mkShell { buildInputs = [ hsenv ]; };
    };
}
