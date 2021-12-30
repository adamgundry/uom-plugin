let defs = ./../defaults.dhall

in  let testopts = [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]

    in    defs
        ⫽ { name = "uom-plugin"
          , synopsis = "Units of measure as a GHC typechecker plugin"
          , description =
              ''
              The @uom-plugin@ library adds support for units of measure to GHC
              using the new experimental facility for typechecker plugins, which
              is available in GHC 7.10 and later.  See
              "Data.UnitsOfMeasure.Tutorial" for an introduction to the library.''
          , category = "Type System"
          , github = "adamgundry/uom-plugin"
          , license = "BSD3"
          , license-file = "LICENSE"
          , stability = "experimental"
          , extra-source-files = [ "CHANGELOG.md", "README.md", "LICENSE" ]
          , library =
            { source-dirs = [ "doc", "src" ]
            , exposed-modules =
              [ "GHC.Corroborate.Type"
              , "Data.UnitsOfMeasure"
              , "Data.UnitsOfMeasure.Convert"
              , "Data.UnitsOfMeasure.Defs"
              , "Data.UnitsOfMeasure.Internal"
              , "Data.UnitsOfMeasure.Plugin"
              , "Data.UnitsOfMeasure.Read"
              , "Data.UnitsOfMeasure.Show"
              , "Data.UnitsOfMeasure.Singleton"
              , "Data.UnitsOfMeasure.Tutorial"
              ]
            , other-modules =
              [ "Data.UnitsOfMeasure.Unsafe.Convert"
              , "Data.UnitsOfMeasure.Unsafe.NormalForm"
              , "Data.UnitsOfMeasure.Unsafe.Unify"
              , "Data.UnitsOfMeasure.TH"
              , "GhcApi"
              ]
            , dependencies =
                  defs.dependencies
                # [ "deepseq >=1.3 && <1.5"
                  , "ghc-tcplugins-extra >=0.1"
                  , "ghc"
                  , "ghc-corroborate"
                  , "template-haskell >=2.9"
                  , "containers >=0.5"
                  , "units-parser >=0.1.1.4"
                  ]
            , when =
              [ { condition = "impl(ghc >= 9.2) && impl(ghc < 9.4)"
                , source-dirs = "src-ghc-9.0"
                }
              , { condition = "impl(ghc >= 9.0) && impl(ghc < 9.2)"
                , source-dirs = "src-ghc-9.0"
                }
              , { condition = "impl(ghc >= 8.10.0) && impl(ghc < 9.0)"
                , source-dirs = "src-ghc-8.10"
                }
              , { condition = "impl(ghc >= 8.8.0) && impl(ghc < 8.10.0)"
                , source-dirs = "src-ghc-8.8"
                }
              , { condition = "impl(ghc >= 8.6.0) && impl(ghc < 8.8.0)"
                , source-dirs = "src-ghc-8.6"
                }
              , { condition = "impl(ghc >= 8.4.0) && impl(ghc < 8.6.0)"
                , source-dirs = "src-ghc-8.4"
                }
              , { condition = "impl(ghc >= 8.2.0) && impl(ghc < 8.4.0)"
                , source-dirs = "src-ghc-8.2"
                }
              , { condition = "impl(ghc >= 8.0.0) && impl(ghc < 8.2.0)"
                , source-dirs = "src-ghc-8.0"
                }
              ]
            }
          , tests =
            { units =
              { dependencies =
                    defs.dependencies
                  # [ "tasty >= 0.11.3"
                    , "tasty-hunit >= 0.9.2"
                    , "ghc-corroborate"
                    , "uom-plugin"
                    ]
              , ghc-options = testopts
              , main = "Tests.hs"
              , source-dirs = "test-suite-units"
              , when =
                { condition = "impl(ghc > 8.2.2) && impl(ghc < 9.2.1)"
                , buildable = False
                }
              }
            , doctest =
              { dependencies =
                    defs.dependencies
                  # [ "deepseq >=1.3 && <1.5"
                    , "ghc"
                    , "ghc-tcplugins-extra >=0.1"
                    , "template-haskell >=2.9"
                    , "containers >=0.5"
                    , "ghc-corroborate"
                    , "units-parser >=0.1"
                    , "doctest"
                    , "QuickCheck"
                    ]
              , ghc-options = [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , main = "DocTest.hs"
              , source-dirs = [ "src", "test-suite-doctest" ]
              }
            }
          }
