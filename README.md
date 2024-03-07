To run doctests:

First make sure `doctest` is on the `PATH` (i.e. `cabal install doctest`).

Then run:

```
cabal repl --with-ghc=doctest --repl-options="-fno-warn-orphans -Wno-x-partial" colonnade
```
