module Main (main) where

import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

arguments :: [String]
arguments =
    [ "lint"
    -- TODO: Remove these ignore flags.
    , "--ignore=Unused LANGUAGE pragma"
    , "--ignore=Parse error"
    , "--ignore=Eta reduce"
    , "--ignore=Use unless"
    , "--ignore=Use foldl"
    , "--ignore=Use =="
    , "--ignore=Use unwords" 
    , "--ignore=Use if" 
    , "--ignore=Use fewer imports" 
    , "--ignore=Redundant $" 
    , "--ignore=Use <$>" 
    , "--ignore=Redundant compare"
    , "src"
    , "tests"
    , "hlint"
    ]

main :: IO ()
main = do
    hints <- hlint arguments
    if null hints then exitSuccess else exitFailure
