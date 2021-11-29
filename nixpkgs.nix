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
  # currently set to match Heaviside's
  sha256 = "02anj9mbzy45bszlmv7k42mb5y7yj2lxc5vpbxgd3f5qljn5ih7y";
  rev = "c00959877fb06b09468562518b408acda886c79e";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
