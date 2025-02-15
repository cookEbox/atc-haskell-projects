{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "16.1";

    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;

    # In order to use Let's Encrypt for HTTPS deployments you must accept
    # their terms of service at https://letsencrypt.org/repository/.
    # Uncomment and set this to `true` to indicate your acceptance:
    # terms.security.acme.acceptTerms = false;
  }
}:
with obelisk;
project ./. ({ pkgs, ... }: {
  overrides = self: super: let 
    aesonSrc = pkgs.fetchFromGitHub {
      owner = "bos";
      repo = "aeson";
      rev = "v2.0.0.0";
      sha256 = "sha256-1cxmxizazi06f82wps7xmxzprn37dzrfzd9rvzy8pqk1gimxa7wm";
    };
    aesonPkg = self.callCabal2nix "aeson" aesonSrc {};
  in 
  { 
    # aeson = aesonPkg;
    inherit (aesonPkg) aeson;
    android.applicationId = "systems.obsidian.obelisk.examples.minimal";
    android.displayName = "Obelisk Minimal Example";
    ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
    ios.bundleName = "Obelisk Minimal Example";
  };
})
