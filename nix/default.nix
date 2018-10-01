{ frontend ? false }:

let _nixpkgs = import <nixpkgs> {};
    nixpkgs = _nixpkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "5c4a404b0d0e5125070dde5c1787210149157e83";
      sha256 = "0a478l0dxzy5hglavkilxjkh45zfg31q50hgkv1npninc4lpv5f7";
    };
    pkgs = import nixpkgs { config = {}; overlays = []; };

    fetch-github-json = owner: repo: path:
      let commit = builtins.fromJSON (builtins.readFile path);
      in pkgs.fetchFromGitHub {
        name = "${repo}-${commit.rev}";
        inherit owner repo;
        inherit (commit) rev sha256;
      };
      
    reflex-platform = import (fetch-github-json "layer-3-communications" "reflex-platform" ./reflex-platform.json) {};
    jsaddle-src = fetch-github-json "ghcjs" "jsaddle" ./jsaddle.json;
    compiler = "ghc8_2_1";

    filterPredicate = p: type:
      let path = baseNameOf p; in !(
           (type == "directory" && pkgs.lib.hasPrefix "dist" path)
        || (type == "symlink"   && pkgs.lib.hasPrefix "result" path)
        || pkgs.lib.hasPrefix ".ghc" path
        || pkgs.lib.hasPrefix ".git" path
        || pkgs.lib.hasSuffix "~" path
        || pkgs.lib.hasSuffix ".o" path
        || pkgs.lib.hasSuffix ".so" path
        || pkgs.lib.hasSuffix ".nix" path);

    overrides = reflex-platform.${compiler}.override {
      overrides = self: super:
        with reflex-platform;
        with reflex-platform.lib;
        with reflex-platform.nixpkgs.haskell.lib;
        with reflex-platform.nixpkgs.haskellPackages; 
        let
          cp              = file: (self.callPackage (./deps + "/${file}.nix") {});
          build-from-json = name: str: self.callCabal2nix name str {};
          build           = name: path: self.callCabal2nix name (builtins.filterSource filterPredicate path) {};
        in
        {
          gtk2hs-buildtools = self.callPackage ./gtk2hs-buildtools.nix {};
          colonnade = build "colonnade" ../colonnade;
          siphon    = build "siphon" ../siphon;
          reflex-dom-colonnade = build "reflex-dom-colonnade" ../reflex-dom-colonnade;
          lucid-colonnade = build "lucid-colonnade" ../lucid-colonnade;
          blaze-colonnade = build "blaze-colonnade" ../blaze-colonnade;
          yesod-colonnade = build "yesod-colonnade" ../yesod-colonnade;
        } //
        {
          jsaddle              = doJailbreak (build-from-json "jsaddle"            "${jsaddle-src}/jsaddle");
          jsaddle-webkitgtk    = doJailbreak (build-from-json "jsaddle-webkitgtk"  "${jsaddle-src}/jsaddle-webkitgtk");
          jsaddle-webkit2gtk   = doJailbreak (build-from-json "jsaddle-webkit2gtk" "${jsaddle-src}/jsaddle-webkit2gtk");
          jsaddle-wkwebview    = doJailbreak (build-from-json "jsaddle-wkwebview"  "${jsaddle-src}/jsaddle-wkwebview");
          jsaddle-clib         = doJailbreak (build-from-json "jsaddle-clib"       "${jsaddle-src}/jsaddle-clib");
          jsaddle-warp         = dontCheck (doJailbreak (build-from-json "jsaddle-warp"       "${jsaddle-src}/jsaddle-warp"));
        };

    };
in rec {
  inherit reflex-platform fetch-github-json overrides nixpkgs pkgs;
  colonnade = overrides.colonnade;
  siphon    = overrides.siphon;
  reflex-dom-colonnade = overrides.reflex-dom-colonnade;
  lucid-colonnade = overrides.lucid-colonnade;
  blaze-colonnade = overrides.blaze-colonnade;
  yesod-colonnade = overrides.yesod-colonnade;
}
