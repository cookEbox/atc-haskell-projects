{
  nixConfig.binaryCaches = [ "https://nixcache.reflex-frp.org" ];
  nixConfig.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.obelisk = {
    url = "github:obsidiansystems/obelisk";
    flake = false;
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import inputs.nixpkgs { inherit system; };
      obelisk = pkgs.callPackage inputs.obelisk {
        inherit system;
        terms.security.acme.acceptTerms = true;
      };
    in
    {
      inherit obelisk;
      devShells.default = pkgs.mkShell {
        nativeBuildInputs = [
          obelisk.command
          pkgs.haskellPackages.haskell-language-server
          pkgs.haskellPackages.implicit-hie
          pkgs.haskellPackages.cabal-install
          pkgs.haskellPackages.ghc
        ];
      };
    });
}
