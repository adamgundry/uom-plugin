{-# LANGUAGE ViewPatterns #-}

module Flight.Ratio (pattern (:%)) where

import Data.Ratio (Ratio, numerator, denominator)

-- | SEE: http://stackoverflow.com/questions/33325370/why-cant-i-pattern-match-against-a-ratio-in-haskell
pattern (:%) :: forall t. t -> t -> Ratio t
pattern num :% denom <- (\x -> (numerator x, denominator x) -> (num, denom))
