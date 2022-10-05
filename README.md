Most of the tests use doctest, which isn't run like a normal test suite (I guess).

To run these tests, first make sure `doctest` is on the `PATH` (i.e. `cabal install doctest`), then run the following commands:

```
cabal repl --build-depends=QuickCheck --with-ghc=doctest --repl-options="-fno-warn-orphans" siphon
cabal repl --build-depends=QuickCheck --with-ghc=doctest --repl-options="-fno-warn-orphans" colonnade
cabal repl --build-depends=QuickCheck --with-ghc=doctest --repl-options="-fno-warn-orphans" blaze-colonnade
```

There are no tests for lucid-colonnade at present.
