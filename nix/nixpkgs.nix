{ compiler ? "ghc822"
, config
}:

# SEE: https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs
let
   hostPkgs = import <nixpkgs> {};
   pinnedVersion = hostPkgs.lib.importJSON ./nixpkgs-version.json;
   pinnedPkgs = hostPkgs.fetchgit {
     inherit (pinnedVersion) url rev sha256;
   };

in
  import pinnedPkgs { inherit config; }
