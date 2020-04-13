module Target where

import Development.Shake (Rules)
import Pkg (buildRules)

allWants :: [ String ]
allWants = ["cabal-files"]

allRules :: Rules ()
allRules = do
    Pkg.buildRules
