# shell.nix
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskellPackages.ghc
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.cabal-install
    pkgs.haskellPackages.zlib_0_7_1_0
    pkgs.zlib
    pkgs.sqlite
    pkgs.rlwrap
  ];
  nativeBuildInputs = [
    pkgs.haskellPackages.haskell-language-server
  ];

  shellHook = ''
    cabal update
  '';
}
