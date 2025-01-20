# shell.nix
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskellPackages.ghc
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.cabal-install
    pkgs.zlib
    pkgs.sqlite.dev
    pkgs.rlwrap
  ];
  nativeBuildInputs = [
    pkgs.haskellPackages.haskell-language-server
    pkgs.zlib.dev
  ];

  shellHook = ''
    cabal update
    export TMPDIR=/tmp
  '';
}
