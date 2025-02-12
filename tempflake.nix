{
  nixConfig.binaryCaches = [ "https://nixcache.reflex-frp.org" ];
  nixConfig.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.obelisk = {
    url = "github:obsidiansystems/obelisk";
    flake = false;
  };

  outputs = {self,nixpkgs,flake-utils,obelisk}: 
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { 
          inherit system; 
          overlays = [
            obelisk.overlay
            (final: prev: {
              persistent = prev.persistent.overrideAttrs (old: {
                meta = old.meta or {} // { broken = false; };
                postPatch = (old.postPatch or "") + ''
                  ${final.doJailbreak}
                '';
              });
            })
          ];
        };

        ob = pkgs.obelisk;

      in
      {
        inherit ob;
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [
            ob.command
            pkgs.haskellPackages.haskell-language-server
            pkgs.haskellPackages.implicit-hie
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.ghc
            pkgs.sqlite
            pkgs.sqlite.dev
          ];
        };
      });
}
