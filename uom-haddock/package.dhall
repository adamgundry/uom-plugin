    let defs = ./defaults.dhall

in    defs
    ⫽ ./default-extensions.dhall
    ⫽ { name =
          "uom-haddock"
      , synopsis =
          "Reproduction of haddock type rename error."
      , description =
          "Reproduction of haddock type rename error."
      , category =
          "Flight"
      , github =
          "blockscope/uom-plugin/uom-plugin-haddock"
      , ghc-options =
          [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
      , dependencies =
            defs.dependencies
          # [ "numbers"
            , "fixed"
            , "bifunctors"
            , "text"
            , "formatting"
            , "uom-plugin"
            , "siggy-chardust"
            ]
      , library =
          { source-dirs =
              "src"
          , exposed-modules =
              [ "Units"
              , "Units.Angle"
              , "Units.DegMinSec"
              ]
          }
      }
