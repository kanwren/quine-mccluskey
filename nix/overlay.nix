self: super:

let
  sources = import ./sources.nix;

  inherit (super) lib;
  hlib = super.haskell.lib;
  clean = import ./clean.nix { inherit (super) lib; };

  ghcOverride = input: ovl: input.override (old: {
    overrides = lib.composeExtensions (old.overrides or (_: _: { })) ovl;
  });

  fixGhcWithHoogle = input: ghcOverride input (hself: hsuper: {
    # Compose the selector with a null filter to fix error on null packages
    ghcWithHoogle = selector:
      hsuper.ghcWithHoogle (ps: builtins.filter (x: x != null) (selector ps));
    ghc = hsuper.ghc // { withHoogle = hself.ghcWithHoogle; };
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
      ghc8101 = fixGhcWithHoogle
        (ghcOverride super.haskell.packages.ghc8101 haskellOverlay);
    };
  };
}
