    let defs = ./../defaults.dhall

in  let testopts = [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]

in    defs
    ⫽ { name =
          "uom-plugin"
      , synopsis =
          "Units of measure as a GHC typechecker plugin"
      , description =
          ''
          The @uom-plugin@ library adds support for units of measure to GHC
          using the new experimental facility for typechecker plugins, which
          is available in GHC 7.10 and later.  See
          "Data.UnitsOfMeasure.Tutorial" for an introduction to the library.''
      , category =
          "Type System"
      , github =
          "adamgundry/uom-plugin"
      , license =
          "BSD3"
      , license-file =
          "LICENSE"
      , stability =
          "experimental"
      , extra-source-files =
          [ "changelog", "README.md", "LICENSE" ]
      , library =
          { source-dirs =
              [ "doc", "src" ]
          , exposed-modules =
              [ "Data.UnitsOfMeasure"
              , "Data.UnitsOfMeasure.Convert"
              , "Data.UnitsOfMeasure.Defs"
              , "Data.UnitsOfMeasure.Internal"
              , "Data.UnitsOfMeasure.Plugin"
              , "Data.UnitsOfMeasure.Read"
              , "Data.UnitsOfMeasure.Show"
              , "Data.UnitsOfMeasure.Singleton"
              , "Data.UnitsOfMeasure.Tutorial"
              ]
          , dependencies =
                defs.dependencies
              # [ "deepseq >=1.3 && <1.5"
                , "ghc >= 8.0.1 && <8.9"
                , "ghc-tcplugins-extra >=0.1"
                , "template-haskell >=2.9"
                , "containers >=0.5"
                , "units-parser >=0.1"
                ]
          }
      , tests =
          { units =
              { dependencies =
                  [ "base"
                  , "uom-plugin"
                  , "tasty >=0.10 && <1.1"
                  , "tasty-hunit >=0.9 && <0.10.1"
                  ]
              , ghc-options =
                  testopts
              , main =
                  "Tests.hs"
              , source-dirs =
                  "test-suite-units"
              , when =
                  { condition = "impl(ghc > 8.2.2)", buildable = False }
              }
          , hlint =
              { dependencies =
                  [ "base", "hlint >=1.7 && <2.3" ]
              , ghc-options =
                  testopts
              , main =
                  "HLint.hs"
              , source-dirs =
                  "test-suite-hlint"
              }
          }
      }
