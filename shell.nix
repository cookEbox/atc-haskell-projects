# shell.nix
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskellPackages.ghc
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.cabal-install
  ];
  nativeBuildInputs = [
    pkgs.haskellPackages.haskell-language-server
  ];

  shellHook = ''
    cabal update
    export TMPDIR=/tmp
  '';
}
