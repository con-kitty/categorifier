# Categorifier

![Build status](https://github.com/con-kitty/categorifier/actions/workflows/ci.yml/badge.svg?branch=master)
[![built with garnix](https://img.shields.io/endpoint?url=https%3A%2F%2Fgarnix.io%2Fapi%2Fbadges%2Fsellout%2Fcategorifier)](https://garnix.io)
[![Packaging status](https://repology.org/badge/tiny-repos/haskell:categorifier.svg)](https://repology.org/project/haskell:categorifier/versions)
[![latest packaged version(s)](https://repology.org/badge/latest-versions/haskell:categorifier.svg)](https://repology.org/project/haskell:categorifier/versions)

Defining novel interpretations of Haskell programs

You probably want to look at the [plugin README](./plugin/README.md).

## Building

A Nix flake is provided, so if you are familiar with Nix, that’s the most reliable way to build the project.

If you‘re not using Nix, the cabal.project file requires at least Cabal 3.8, but the individual projects should work with older versions.

## Contributing

There are compatible [direnv](https://direnv.net/) and [Nix](https://nixos.org/manual/nix/stable/) environments in the repository to make it easy to build, test, etc. everything with consistent versions to help replicate issues.

This repository is all formatted using [Ormolu](https://github.com/tweag/ormolu). Currently CI runs Ormolu 0.4.0.0, which can be installed by `cabal install ormolu-0.4.0.0`. See the [usage notes](https://github.com/tweag/ormolu#usage) for how to best integrate it with your workflow. But don't let Ormolu get in the way of contributing - CI will catch the formatting, and we can help clean up anything.
