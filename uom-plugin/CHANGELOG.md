# Changelog

## [0.5.0.0] Not yet released
* Significant API redesign to use data types as units, rather than Symbols
* Drop proxy arguments from various functions

## [0.4.0.0] 2022-10-08
* Support building on GHC 9.0, 9.2 and 9.4 (thanks to Phil de Joux and Sam Derbyshire)
* Drop support for all previous versions of GHC
* Doctest the tutorial
* Various bug fixes and minor tweaks

## [0.3.0.0] 2018-07-13
### Added
* Support building on GHC 8.2 (but not yet 8.4)
* Expose toRational' in Data.UnitsOfMeasure
* An hlint test suite (thanks to Phil de Joux)
* Packaging improvements

### Fixed
* Fix unit safety bug in GHC 8.0 and later (see #22)

## [0.2.0.1] 2016-05-10
* Support building on GHC 8.0

## [0.2.0.0] 2015-12-31
### Added
* Data.UnitsOfMeasure.Read module and a Read instance for Quantity
* Make it possible to declare derived compound units
* Define litres, hectares, radians and steradians

### Fixed
* Prevent cyclic definitions of convertible units

## [0.1.1.0] 2015-11-27
### Added
* More conversion ratios (thanks to Joe Hermaszewski)
* Storable and NFData instances for Quantity (thanks to Marcin Mrotek)

## [0.1.0.0] 2015-08-19
* First public release
