A small server for exposing a few shell commands and passing them an argument.

# Installation

## via [Haskell](https://www.haskell.org/downloads#minimal)

```bash
cabal sandbox init
cabal install --only-dep
cabal install
```

# Running locally

```bash
.cabal-sandbox/bin/shell-base-server -p 8000
```

Run as root on port 80 instead of 8000 for public access.
