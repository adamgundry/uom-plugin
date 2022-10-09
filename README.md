# Units of measure as a GHC type-checker plugin

:warning: This library is experimental, and may lead to unexpected type-checking failures or even type soundness bugs.

The `uom-plugin` library adds support for units of measure as a GHC type-checker plugin.  See [Data.UnitsOfMeasure.Tutorial](https://hackage.haskell.org/package/uom-plugin/docs/Data-UnitsOfMeasure-Tutorial.html) for an introduction to the library, and [the accompanying paper](http://adam.gundry.co.uk/pub/typechecker-plugins/) for more background.  An example of a package that uses the library is given in [uom-plugin-examples](https://github.com/adamgundry/uom-plugin/tree/master/uom-plugin-examples).

The latest version of the library is tested with GHC 9.0 to 9.4. Older versions of `uom-plugin` (0.3 and earlier) work with the GHC 7.10, 8.0 and 8.2 series. There are no versions supporting GHC 8.4 to 8.10 ([#43][i43]). Running `cabal haddock` on this library requires GHC 9.4 (see [#66][i66]).

[i43]: https://github.com/adamgundry/uom-plugin/issues/43
[i66]: https://github.com/adamgundry/uom-plugin/issues/66

## Installation

```
> git clone https://github.com/adamgundry/uom-plugin.git
> cd uom-plugin
> cabal new-build all
> cabal new-run Examples
```

### Continuous Integration

![Build Status](https://github.com/adamgundry/uom-plugin/actions/workflows/haskell-ci.yml/badge.svg)

Generate the `.github/worksflows/haskell-ci.yml` setup with:

```
> haskell-ci github cabal.project --local-ghc-options -dcore-lint
```
