let
  sources = import ./sources.nix;

  pkgs = import sources.nixpkgs {
    overlays = [ (import ./overlay.nix) ];
  };

  hpkgs = pkgs.haskell.packages.ghc883;
in

rec {
  inherit (hpkgs) quine-mccluskey;

  shell = hpkgs.shellFor {
    packages = ps: with ps; [ quine-mccluskey ];

    buildInputs = (with pkgs; [
      # Needed for niv
      niv nix cacert
    ]) ++ (with hpkgs; [
      ghcid
      cabal-install
    ]);

    exactDeps = true;
  };
}
