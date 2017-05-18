import Control.Monad.Trans.Writer

import Disclaimer
import Lenin

main :: IO ()
main = do
    print $ runWriter $ safeRectArea  2   (20 + lenin)
    print $ runWriter $ safeRectArea 13 5000
