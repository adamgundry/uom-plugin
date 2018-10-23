# Units of measure as a GHC typechecker plugin

The `uom-plugin` library adds support for units of measure to GHC using the new experimental facility for typechecker plugins, which is available in GHC `7.10` and later.  See [Data.UnitsOfMeasure.Tutorial](https://github.com/adamgundry/uom-plugin/blob/master/uom-plugin/src/Data/UnitsOfMeasure/Tutorial.hs) for an introduction to the library, and [the accompanying paper](http://adam.gundry.co.uk/pub/typechecker-plugins/) for more background.  An example of a package that uses the library is given in [uom-plugin-examples](uom-plugin-examples).

The library has been tested with GHC `7.10.3`, `8.0.2` & `8.2.2`.

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

[![Build Status](https://travis-ci.org/adamgundry/uom-plugin.svg?branch=master)](https://travis-ci.org/adamgundry/uom-plugin)
