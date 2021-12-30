module Main (main) where

import Test.DocTest (doctest)

arguments :: [String]
arguments =
    [ "-idoc-ghc-8.2"
    , "-isrc"
    , "./src/Plugins/UoM/UnitDefs.hs"
    , "./doc-ghc-8.2/Data/UnitsOfMeasure/Tutorial.hs"
    , "-fplugin Data.UnitsOfMeasure.Plugin"
    ]

main :: IO ()
main = doctest arguments
