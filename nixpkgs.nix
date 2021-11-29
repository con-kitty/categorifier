/*
NIXPKGS version
Any archive of nixpkgs can be used.
The simplest update solution is to look at
http://github.com/NixOS/nixpkgs and pick the latest commit for
the desired branch. The archive can then be fetched at:
https://github.com/NixOS/nixpkgs/archive/COMMIT_NUMBER.tar.gz;
and the control sum computed using `sha256`.
*/

let
  # nixpkgs-21.05 2021-11-28
  sha256 = "1w8d2q2n0s8b3wjwvysrxcx3yr3dyna8144p64490a77r61d17y2";
  rev = "4f37689c8a219a9d756c5ff38525ad09349f422f";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
