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
  # release-21.11
  sha256 = "1llhpnpfcvvfcr6s2y8qa9liqln7090brgaw6b4hbc8zac2zs55d";
  rev = "216c78db4d10e85b324acca9ac0d1af7f2df2031";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
