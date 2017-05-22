module SimpleCheck where

import SimpleWriter

-- | Check a number, potentially changing it.
-- The returned Writer may contain comments.
checkInt :: Int -> Writer Int
checkInt x = writer (n, msg)
  where
    (n, msg) | x  <    0 = (   0, [show x ++ " Negative, but not my fault"     ])
             | x ==   13 = (   7, [show x ++ " Boss hates 13, 7 is better"     ])
             | x  > 1999 = (1999, [show x ++ " can cause Y2K bug. Back to 1999"])
             | otherwise = (   x, []                                            )
