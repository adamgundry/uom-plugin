module Main (main) where

import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

arguments :: [String]
arguments =
    [ "lint"
    , "--ignore=Parse error"
    , "--ignore=Use fewer imports"  -- This is a pain for the CPP in TcPluginExtras
    , "src"
    , "tests"
    , "hlint"
    ]

main :: IO ()
main = do
    hints <- hlint arguments
    if null hints then exitSuccess else exitFailure
