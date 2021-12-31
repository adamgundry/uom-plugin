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
            { source-dirs = [ "src" ]
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
              , "Data.UnitsOfMeasure.TH"
              ]
            , other-modules =
              [ "Data.UnitsOfMeasure.Unsafe.Convert"
              , "Data.UnitsOfMeasure.Unsafe.NormalForm"
              , "Data.UnitsOfMeasure.Unsafe.Unify"
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
                , source-dirs = [ "src-ghc-9.0", "doc-ghc-9.2" ]
                }
              , { condition = "impl(ghc >= 9.0) && impl(ghc < 9.2)"
                , source-dirs = [ "src-ghc-9.0", "doc-ghc-9.2" ]
                }
              , { condition = "impl(ghc >= 8.10.0) && impl(ghc < 9.0)"
                , source-dirs = [ "src-ghc-8.10" ]
                }
              , { condition = "impl(ghc >= 8.8.0) && impl(ghc < 8.10.0)"
                , source-dirs = [ "src-ghc-8.8" ]
                }
              , { condition = "impl(ghc >= 8.6.0) && impl(ghc < 8.8.0)"
                , source-dirs = [ "src-ghc-8.6" ]
                }
              , { condition = "impl(ghc >= 8.4.0) && impl(ghc < 8.6.0)"
                , source-dirs = [ "src-ghc-8.4" ]
                }
              , { condition = "impl(ghc >= 8.2.0) && impl(ghc < 8.4.0)"
                , source-dirs = [ "src-ghc-8.2", "doc-ghc-8.2" ]
                }
              , { condition = "impl(ghc >= 8.0.0) && impl(ghc < 8.2.0)"
                , source-dirs = [ "src-ghc-8.0", "doc-ghc-8.2" ]
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
              , other-modules = [ "UnitDefs", "UnitDefsTests", "Z" ]
              , source-dirs = "test-suite-units"
              , when =
                [ { condition = "impl(ghc >= 9.2) && impl(ghc < 9.4)"
                  , source-dirs = [ "test-suite-units-ghc-9.2" ]
                  , other-modules = [ "ErrorTests", "ErrorTestGroups" ]
                  , buildable = True
                  }
                , { condition = "impl(ghc >= 8.2) && impl(ghc < 9.2)"
                  , source-dirs = [ "test-suite-units-ghc-8.2" ]
                  , other-modules = [ "ErrorTests", "ErrorTestGroups" ]
                  , buildable = True
                  }
                , { condition = "impl(ghc >= 8.4) && impl(ghc < 9.2)"
                  , source-dirs = [] : List Text
                  , other-modules = [] : List Text
                  , buildable = False
                  }
                ]
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
              , main = "DocTest.hs"
              , source-dirs = [ "src" ]
              , other-modules =
                [ "GHC.Corroborate.Type"
                , "Data.UnitsOfMeasure.Unsafe.Convert"
                , "Data.UnitsOfMeasure.Unsafe.NormalForm"
                , "Data.UnitsOfMeasure.Unsafe.Unify"
                , "GhcApi"
                ]
              , ghc-options = [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , when =
                [ { condition = "impl(ghc >= 9.2) && impl(ghc < 9.4)"
                  , source-dirs =
                    [ "src-ghc-9.0", "test-suite-doctest-ghc-9.2" ]
                  , other-modules = [] : List Text
                  }
                , { condition = "impl(ghc >= 8.2) && impl(ghc < 9.2)"
                  , source-dirs =
                    [ "src-ghc-8.2", "test-suite-doctest-ghc-8.2" ]
                  , other-modules = [] : List Text
                  }
                ]
              }
            }
          }
