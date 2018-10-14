{ compiler ? "ghc822"
}:

# NOTE: For adding overrides without nuking previous overrides with
# overrides = lib.composeExtensions (old.overrides or (_: _: {})) (self: super:
# SEE: https://github.com/NixOS/nixpkgs/issues/26561
# NOTE: Use super for super.callPackage but self for self.package.
# self: Fix-point result.
# super: Result of the composition beforehand.
# SEE: https://nbp.github.io/slides/NixCon/2017.NixpkgsOverlays/
{
  allowUnsupportedSystem = true;
  allowUnfree = true;

  packageOverrides = pkgs:
    let old = pkgs.haskell.packages.${compiler}; in rec {
    haskellPackages = pkgs.haskell.packages.${compiler}.override {
      overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super:
      {
        units-parser = super.callPackage ./units-parser.nix {};

        uom-plugin = super.callPackage ../uom-plugin/uom-plugin.nix
              { units-parser = self.units-parser; };

      });
    };
  };
}
