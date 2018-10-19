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
          "blockscope/uom-plugin/uom-haddock"
      , dependencies =
            defs.dependencies
          # [ "uom-plugin" ]
      , library =
          { source-dirs =
              "src"
          , exposed-modules =
              [ "Units.Angle" ]
          }
      }
