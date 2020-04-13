    let defs = ./../defaults.dhall

in  let testopts =
          [ "-rtsopts"
          , "-threaded"
          , "-with-rtsopts=-N"
          , "-fplugin Data.UnitsOfMeasure.Plugin"
          ]

in    defs
    ⫽ { name =
          "uom-plugin-tutorial"
      , synopsis =
          "A tutorial for the Units of measure typechecker plugin"
      , description =
          "A tutorial for the units of measure typechecker plugin, checked with doctest."
      , category =
          "Type System"
      , github =
          "adamgundry/uom-plugin"
      , license =
          "BSD3"
      , license-file =
          "LICENSE"
      , tests =
          { doctest =
              { dependencies =
                  [ "base", "uom-plugin", "doctest" ]
              , ghc-options =
                  testopts
              , main =
                  "DocTest.hs"
              , source-dirs =
                  [ "test-suite-doctest" ]
              , when =
                  { condition = "impl(ghc > 8.2.2)", buildable = False }
              }
          }
      }
