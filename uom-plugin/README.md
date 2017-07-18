Both test suites can be run together or individually.
```
$ stack test
uom-plugin-0.2.0.1: test (suite: hlint)
uom-plugin-0.2.0.1: test (suite: units)
```
```
$ stack test uom-plugin:units 
uom-plugin-0.2.0.1: test (suite: units)
```
```
$ stack test uom-plugin:hlint
uom-plugin-0.2.0.1: test (suite: hlint)
```
