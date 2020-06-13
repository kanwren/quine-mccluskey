self: super:

let
  sources = import ./sources.nix;

  inherit (super) lib;
  hlib = super.haskell.lib;
  clean = import ./clean.nix { inherit (super) lib; };

  ghcOverride = input: ovl: input.override (old: {
    overrides = lib.composeExtensions (old.overrides or (_: _: { })) ovl;
  });

  # Package overrides
  packageOverlay = hself: hsuper: { };

  # Result packages
  mainOverlay = hself: hsuper: {
    quine-mccluskey = hsuper.callCabal2nix "quine-mccluskey" (clean ../.) { };
  };

  composeOverlays = lib.foldl' lib.composeExtensions (_: _: { });
  haskellOverlay = composeOverlays [ mainOverlay packageOverlay ];

in {
  niv = (import sources.niv {}).niv;

  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghc883 = ghcOverride super.haskell.packages.ghc883 haskellOverlay;
    };
  };
}
