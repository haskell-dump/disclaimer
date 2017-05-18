module Main where

import Disclaimer
import Lenin

import Control.Monad.Trans.Writer

main :: IO ()
main = do
    print $ runWriter $ safeRectArea  2 (20 + lenin)
    print $ runWriter $ safeRectArea 13 5000
