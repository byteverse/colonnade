{ options ? (x: x), filterPredicate ? (x: true), lib, cabal2nixResult, self, super }:
let build = path: options (self.callPackage (cabal2nixResult (builtins.filterSource filterPredicate path)) {});
in {
  # Core Libraries
  colonnade = lib.dontCheck (build ../colonnade);
  reflex-dom-colonnade = build ../reflex-dom-colonnade;
}
