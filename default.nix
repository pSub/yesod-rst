{ compiler ? "ghc94", nixpkgs ? import <nixpkgs> {} }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./yesod-rst.nix { }