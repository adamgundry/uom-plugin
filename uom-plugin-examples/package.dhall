let defs = ./../defaults.dhall

in  let testopts = [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]

    in    defs
        ⫽ { ghc-options = "-dcore-lint" }
        ⫽ { name = "uom-plugin-examples"
          , synopsis = "Examples of the use of uom-plugin"
          , description =
              "This package provides examples of the use of uom-plugin"
          , license = "PublicDomain"
          , executables.Examples
            =
            { dependencies = defs.dependencies # [ "uom-plugin" ]
            , source-dirs = "."
            , main = "Examples.hs"
            , when =
              [ { condition = "impl(ghc < 8.4)", buildable = True }
              , { condition = "impl(ghc >= 9.2.1)", buildable = False }
              , { condition = "impl(ghc >= 8.4) && impl(ghc < 9.2)"
                , buildable = False
                }
              ]
            }
          }
