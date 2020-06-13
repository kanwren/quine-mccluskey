{ lib }:

path:

if !lib.canCleanSource path
then path
else lib.cleanSourceWith {
  src = lib.cleanSource path;
  filter = name: type:
    let
      baseName = baseNameOf (toString name);
      # Filetypes to ignore, e.g. ".nix"
      ignoreExts = [];
    in !lib.any (x: x) [
      ((type == "regular") && (lib.any (ext: lib.hasSuffix ext baseName) ignoreExts))
      # git files
      ((type == "directory") && (baseName == ".git"))
      ((type == "regular") && (baseName == ".gitignore"))
      # ghc output
      ((type == "directory") && (baseName == "dist" || baseName == "dist-newstyle"))
      ((type == "regular") && (lib.hasPrefix ".ghc" baseName))
      # other output
    ];
}

