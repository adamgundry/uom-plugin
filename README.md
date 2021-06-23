# Units of measure as a GHC typechecker plugin

The `uom-plugin` library adds support for units of measure to GHC as a type-checker plugin.  See [Data.UnitsOfMeasure.Tutorial](https://github.com/adamgundry/uom-plugin/blob/master/uom-plugin/src/Data/UnitsOfMeasure/Tutorial.hs) for an introduction to the library, and [the accompanying paper](http://adam.gundry.co.uk/pub/typechecker-plugins/) for more background.  An example of a package that uses the library is given in [uom-plugin-examples](uom-plugin-examples).

The latest version of the library is tested with GHC 9.0.1, and should work with 9.2.1. Older versions of `uom-plugin` (0.3 and earlier) work with the GHC 7.10, 8.0 and 8.2 series. There are no versions supporting GHC 8.4 to 8.10 ([#43][i43]).

[i43]: https://github.com/adamgundry/uom-plugin/issues/43

## Installation

Grab the source.

```
> git clone https://github.com/adamgundry/uom-plugin.git
> cd uom-plugin
```

Then build and run with any of these methods:

* ### With Cabal new-build

```
> cabal new-build all
> cabal new-exec Examples
```

* ### With Nix and Cabal

The default compiler is set in `./nix/config.nix` as `compiler ? "ghc822"`;

```
> nix-shell

[nix-shell:]$ cabal new-build all
Resolving dependencies...
Up to date
```

With `GHC > 8.2.2` only the plugin itself builds, not its tests or the
examples. There are conditionals for this in the `package.dhall` files;

```
# package.dhall
    when = { condition = "impl(ghc > 8.2.2)", buildable = False }

# .cabal
    if impl(ghc > 8.2.2)
      buildable: False
```

If the conditions were not in place then what they do is equivalent to;

```
> nix-shell -p haskell.compiler.ghc822 -p haskell.packages.ghc822.cabal-install

[nix-shell:]$ cabal new-build uom-plugin --disable-tests
Resolving dependencies...
Build profile: -w ghc-8.2.2 -O1
...
```

* ### Within a Cabal Sandbox

```
> cabal sandbox init
(cd uom-plugin && cabal sandbox init --sandbox=../.cabal-sandbox && cabal install)
> cd uom-plugin-examples/
uom-plugin-examples> cabal sandbox init --sandbox=../.cabal-sandbox
uom-plugin-examples> cabal build
uom-plugin-examples> dist/build/Examples/Examples
```

* ### With Stack

From the set of `./stack/stack-<ghc-version>.yaml` provided files, picking the
one that corresponds to ghc-8.2.2;

```
> stack build --stack-yaml=./stack/stack-8.2.2.yaml
> stack exec Examples --stack-yaml=./stack/stack-8.2.2.yaml
```

### Continuous Integration

![Build Status](https://github.com/adamgundry/uom-plugin/actions/workflows/haskell-ci.yml/badge.svg)

Generate the `.github/worksflows/haskell-ci.yml` setup with;

```
> haskell-ci github cabal.project --local-ghc-options -dcore-lint
```
