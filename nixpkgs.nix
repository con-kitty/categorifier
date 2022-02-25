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
  sha256 = "0bfz61m3ny0akvqa1nkhc1i5p4rrbs239x68wzm6a8mfyqf1zbnb";
  rev = "a22bd30538d7e9274c0fc6a2254ed92ff340b484";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
