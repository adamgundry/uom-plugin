# Units of measure as a GHC typechecker plugin

The `uom-plugin` library adds support for units of measure to GHC using the new experimental facility for typechecker plugins, which is available in GHC `7.10` and later.  See [Data.UnitsOfMeasure.Tutorial](https://github.com/adamgundry/uom-plugin/blob/master/uom-plugin/src/Data/UnitsOfMeasure/Tutorial.hs) for an introduction to the library, and [the accompanying paper](http://adam.gundry.co.uk/pub/typechecker-plugins/) for more background.  An example of a package that uses the library is given in [uom-plugin-examples](uom-plugin-examples).

The library has been tested with GHC `7.10.3`, `8.0.2` & `8.2.2`.

:warning: Please don't use this plugin with later versions of GHC as it will likely fail to solve constraints, [#43][i43].

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

From the set of `stack-<ghc-version>.yaml` provided files, picking the one that
corresponds to ghc-8.2.2;

```
> stack build --stack-yaml=stack-8.2.2.yaml
> stack exec Examples --stack-yaml=stack-8.2.2.yaml
```

### Continuous Integration

[![Build Status](https://travis-ci.org/adamgundry/uom-plugin.svg?branch=master)](https://travis-ci.org/adamgundry/uom-plugin)

If installing tooling from the master branch of haskell-ci then generate the
`.travis.yml` setup with;

```
> make-travis-yml --output=.travis.yml --config=cabal.haskell-ci cabal.project
```

If installing tooling from the package branch of haskell-ci then generate the
`.travis.yml` setup with;

```
> haskell-ci --output=.travis.yml --config=cabal.haskell-ci cabal.project
```
