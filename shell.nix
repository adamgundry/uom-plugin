let
  config = import ./nix/config.nix {};
  pkgs = import ./nix/nixpkgs.nix { inherit config; };
in
  import uom-plugin/drv.nix { nixpkgs = pkgs; }
