let defs = ./../defaults.dhall

in  let testopts =
          [ "-rtsopts"
          , "-threaded"
          , "-with-rtsopts=-N"
          , "-fplugin Data.UnitsOfMeasure.Plugin"
          ]

    in    defs
        ⫽ { name = "uom-plugin-tutorial"
          , synopsis = "A tutorial for the Units of measure typechecker plugin"
          , description =
              "A tutorial for the units of measure typechecker plugin, checked with doctest."
          , category = "Type System"
          , license = "BSD3"
          , tests.doctest
            =
            { dependencies =
                defs.dependencies # [ "uom-plugin", "doctest >= 0.13.0" ]
            , ghc-options = testopts
            , main = "DocTest.hs"
            , source-dirs = [ "src" ]
            , exposed-modules = [] : List Text
            , other-modules = [ "Plugins.UoM.UnitDefs" ]
            , when =
              [ { condition = "impl(ghc >= 9.2) && impl(ghc < 9.4)"
                , source-dirs = [ "doc-ghc-9.2", "test-suite-doctest-ghc-9.2" ]
                , other-modules = [ "Data.UnitsOfMeasure.Tutorial" ]
                }
              , { condition = "impl(ghc >= 8.2) && impl(ghc < 9.2)"
                , source-dirs = [ "doc-ghc-8.2", "test-suite-doctest-ghc-8.2" ]
                , other-modules = [ "Data.UnitsOfMeasure.Tutorial" ]
                }
              ]
            }
          }
