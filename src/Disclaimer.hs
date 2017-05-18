module Disclaimer where

import Control.Applicative
import Control.Monad.Trans.Writer
import Data.Function

-- | A lawyer-friendly container for minimal liability.
-- The value may contain any finite ammount of disclaimers.
type Disclaim a = Writer [String] a

-- | Calculate the area of a rectangle.
rectArea :: Int -> Int -> Int
rectArea = (*)

-- | Job-keeping version of 'rectArea', for highly
-- unreliable input. Avoids any liability regarding
-- invalid legacy data.
safeRectArea :: Int -> Int -> Disclaim Int
safeRectArea = liftA2 rectArea `on` fixLegacyNum

-- | Check, and possibly correct, an invalid number,
-- adding a disclaimer to avoid future liabilities.
-- The values are adjusted according to internal,
-- mostly undocumented guidelines, carefully designed
-- by interns fighting deadlines to produce verisimilar
-- fake reports.
fixLegacyNum :: Int -> Disclaim Int
fixLegacyNum x = writer (n, msg)
  where
    (n, msg) | x  <    0 = (   0, [show x ++ " Negative, but not my fault"     ])
             | x ==   13 = (   7, [show x ++ " Boss hates 13, 7 is better"     ])
             | x  > 1999 = (1999, [show x ++ " can cause Y2K bug. Back to 1999"])
             | otherwise = (   x, mempty                                        )
