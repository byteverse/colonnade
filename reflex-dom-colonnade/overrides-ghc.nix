{ reflex-platform, ... }:
let dc = reflex-platform.nixpkgs.haskell.lib.dontCheck;
in reflex-platform.ghc.override {
  overrides = self: super: {
    colonnade = dc (self.callPackage (reflex-platform.cabal2nixResult ../colonnade) {});
  };
}
