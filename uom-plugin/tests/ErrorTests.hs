{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

#if __GLASGOW_HASKELL__ > 710
{-# OPTIONS_GHC -fno-warn-deferred-type-errors #-}
#endif

module ErrorTests where

import Data.UnitsOfMeasure
import Data.UnitsOfMeasure.Defs

mismatch1 :: Quantity Double [u| s/m |]
mismatch1 = [u| 3 m/s |]

mismatch1_errors = [ [ "Couldn't match type ‘Base \"s\" /: Base \"m\"’"
                     , "with ‘Base \"m\" /: Base \"s\"’" ]
                   , [ "Couldn't match type ‘Base \"m\" /: Base \"s\"’"
                     , "with ‘Base \"s\" /: Base \"m\"’" ]
                   ]


mismatch2 = [u| 2 m |] +: ([u| 2 s |] :: Quantity Int [u| s |])

mismatch2_errors = [ [ "Couldn't match type ‘Base \"s\"’ with ‘Base \"m\"’" ]
                   , [ "Couldn't match type ‘Base \"m\"’ with ‘Base \"s\"’" ]
                   ]


given1 :: ((One *: a) ~ (a *: One)) => Quantity Double a -> Quantity Double [u|kg|]
given1 = id

given1_errors = [ [ "Could not deduce (a ~ Base \"kg\")"
                  , "from the context ((One *: a) ~ (a *: One))" ]
                , [ "Could not deduce: a ~ Base \"kg\""
                  , "from the context: (One *: a) ~ (a *: One)" ]
                , [ "Could not deduce: Base \"kg\" ~ a"
                  , "from the context: (One *: a) ~ (a *: One)" ]
                ]


given2 :: ((One *: a) ~ (b *: One)) => Quantity Double a -> Quantity Double [u|kg|]
given2 = id

given2_errors = [ [ "Could not deduce (a ~ Base \"kg\")"
                  , "from the context ((One *: a) ~ (b *: One))" ]
                , [ "Could not deduce: a ~ Base \"kg\""
                  , "from the context: (One *: a) ~ (b *: One)" ]
                , [ "Could not deduce: Base \"kg\" ~ a"
                  , "from the context: (One *: a) ~ (b *: One)" ]
                ]


given3 :: ((a ^: 2) ~ (b ^: 3)) => Quantity Integer b -> Quantity Integer a
given3 _ = [u| 3 s |]

given3_errors = [ [ "Could not deduce (a ~ Base \"s\")"
                  , "from the context ((a ^: 2) ~ (b ^: 3))" ]
                , [ "Could not deduce: a ~ Base \"s\""
                  , "from the context: (a ^: 2) ~ (b ^: 3)" ]
                , [ "Could not deduce: Base \"s\" ~ a"
                  , "from the context: (a ^: 2) ~ (b ^: 3)" ]
                ]
