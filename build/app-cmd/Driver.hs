module Driver where

import Development.Shake (shakeArgs, shakeOptions, want)
import Target (allWants, allRules)

drive :: IO ()
drive = shakeArgs shakeOptions $ do

    want allWants
    allRules
