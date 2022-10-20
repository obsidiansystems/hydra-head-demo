# default.nix
{ system ? builtins.currentSystem }:
let
  nix-thunk = import ./dep/nix-thunk { };
  sources = nix-thunk.mapSubdirectories nix-thunk.thunkSource ./dep;
  reflex-platform = import sources.reflex-platform { inherit system; };
  hydra-poc = import sources.hydra-poc { };
in reflex-platform.project ({ pkgs, ... }:
  let haskellLib = pkgs.haskell.lib;
  in {
    packages = { hydra-head-demo = ./.; };

    shellToolOverrides = ghc: super: {
    };

    overrides = self: super: {
      hydra-head-demo = haskellLib.overrideCabal super.hydra-head-demo (drv: {
        librarySystemDepends = (drv.librarySystemDepends or [ ]) ++ [
          hydra-poc.cardano-node.cardano-node
          hydra-poc.cardano-node.cardano-cli
          hydra-poc.hsPkgs.hydra-node.components.exes.hydra-node
          hydra-poc.hsPkgs.hydra-node.components.exes.hydra-tools
          pkgs.jq
          pkgs.coreutils
        ];
      });
    };

    useWarp = true;

    withHoogle = true;

    shells = {
      ghc = [ "hydra-head-demo" ];
      ghcjs = [ ];
    };
  })
