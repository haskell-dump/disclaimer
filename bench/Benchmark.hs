import Disclaimer

import Criterion.Main

main = defaultMain
    [ bgroup "rectArea"
        [ bench "unit square"     $ whnf (rectArea     1)     1
        , bench "large rectangle" $ whnf (rectArea 12345) 67890 ]
    ]
