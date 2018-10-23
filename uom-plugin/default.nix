let
  config = import ../nix/config.nix {};
  pkgs = import ../nix/nixpkgs.nix { inherit config; };
in
  { uom-plugin = pkgs.haskellPackages.uom-plugin; }
