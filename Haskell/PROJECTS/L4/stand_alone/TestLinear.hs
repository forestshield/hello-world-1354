module Main where

import Linear
import Physics.Learn.QuantumMat

main = do
    print $ V0

    print $ V1 1
    print $ V1 1 + V1 2
    print $ V1 1 - V1 2
    print $ V1 1 * V1 2
    print $ V1 1 / V1 2

    print $ V2 1 2
    print $ V2 1 2 + V2 3 4
    print $ V2 1 2 - V2 3 4
    print $ V2 1 2 * V2 3 4
    print $ V2 1 2 / V2 3 4
    print $ perp $ V2 0 1

    print $ V3 1 2 3
    print $ V3 1 2 3 + V3 4 5 6
    print $ V3 1 2 3 - V3 4 5 6
    print $ V3 1 2 3 * V3 4 5 6
    print $ V3 1 2 3 / V3 4 5 6
    print $ cross (V3 1 2 3) (V3 4 5 6)

    print $ V4 1 2 3 4
    print $ V4 1 2 3 4 + V4 5 6 7 8
    print $ V4 1 2 3 4 - V4 5 6 7 8
    print $ V4 1 2 3 4 * V4 5 6 7 8
    print $ V4 1 2 3 4 / V4 5 6 7 8
    print $ vector $ V3 1 2 3
    print $ point $ V3 1 2 3

    print $ (zero :: V3 Double)
    print $ negated $ V3 1 2 3
    print $ V3 1 2 3 ^* V3 4 5 6
    print $ V3 1 2 3 *^ V3 4 5 6
    print $ V3 1 2 3 ^/ 2
    print $ sumV [V3 1 2 3, V3 4 5 6, V3 7 8 9]
    print $ (basis :: [V3 Int])
    print $ basisFor $ V3 1 2 3
    --print $ kronecker $ V3 1 2 3
    print $ outer (V3 1 2 3) (V3 4 5 6)

    print $ nearZero (1e-10 :: Double)
    print $ nearZero (1e-15 :: Double)

    print $ Linear.trace $ V3 (V3 1 2 3) (V3 4 5 6) (V3 7 8 9)
    print $ diagonal $ V3 (V3 1 2 3) (V3 4 5 6) (V3 7 8 9)

    print $ dot (V3 1 2 3) (V3 4 5 6)
    print $ quadrance $ V3 1 2 3
    print $ qd (V3 1 2 3) (V3 4 5 6)
    print $ distance (V3 1 2 3) (V3 4 5 6)
    print $ Linear.norm $ V3 1 2 3
    print $ signorm $ V3 1 2 3
    print $ Linear.normalize (V3 1 2 3 :: V3 Double)
