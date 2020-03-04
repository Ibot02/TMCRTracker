{ system ? builtins.currentSystem }:
(import ./reflex-platform {
  inherit system;
  config.android_sdk.accept_license = true;
}).project ({ pkgs, ...}: {
  useWarp = true;
  packages = {
    common = ./common;
    tracker = ./tracker;
    tmcr = ./tmcr;
    frontend = ./frontend;
  };
  
  shells = {
    ghc = ["common" "tracker" "tmcr" "frontend"];
    ghcjs = ["common" "tracker" "tmcr" "frontend"];
  };

  android.frontend = {
    executableName = "frontend";
    applicationId = "net.ibot02.tracker";
    displayName = "Tracker";
  };

  overrides = self: super: {
    #text = self.callHackage "text" "1.2.4.0" {}; #provides a Lift instance for Text, didn't build
  };
})
