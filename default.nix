let
  # Pin haskell-nix to a recent (as of 2022/11/15) commit
  h8x-pin = "https://github.com/input-output-hk/haskell.nix/archive/c43557a037fbc5f1040a7c30e22aa2760339aa81.tar.gz";
  h8x-src = builtins.fetchTarball h8x-pin;
  h8x = import h8x-src {};

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import

    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these. But you
    # can also just use your own, e.g. '<nixpkgs>'.

    h8x.sources.nixpkgs-2205

    # These arguments passed to nixpkgs, include some patches and also the
    # haskell.nix functionality itself as an overlay.

    h8x.nixpkgsArgs;

in pkgs.haskell-nix.cabalProject {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "redis-glob";
    src = ./.;

  };

  # Specify the GHC version to use.
  compiler-nix-name = "ghc8107";

  # Specify the hackage index state
  index-state = "2022-11-14T00:00:00Z";
}
