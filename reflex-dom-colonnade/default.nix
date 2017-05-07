{ test ? "true" }:
let parseBool = str: with builtins;
  let json = fromJSON str; in if isBool json then json else throw "nix parseBool: ${str} is not a bool.";
in
import ../nix/default.nix { package = "reflex-dom-colonnade"; frontend = false; test = parseBool test; }
