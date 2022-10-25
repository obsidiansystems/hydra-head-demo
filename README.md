# <p align="center">Hydra Head Demo, Reflex-DOM edition</p>

This is an extended, graphical re-implementation of the original [Hydra Heads demo](https://github.com/input-output-hk/hydra-poc/tree/master/demo) implemented with [Reflex FRP](https://reflex-frp.org/).
It allows starting and closing a head with an arbitrary number of nodes, each with some initial amount of Ada to perform transactions within the head.

# Running

To run the demo enter a Nix shell and run it with `cabal`:

```
 $ nix-shell -A shells.ghc default.nix
 $ cabal run
```

The demo can then be viewed in **Chrome**(*Chrome must be used at this time because of a limitation in JSaddle*) at `http://localhost:3003/`.

