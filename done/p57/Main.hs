module Main where

import qualified GHC.Real as Real

half = 1 / 2 :: Rational

values = map (+1) $ iterate ((1/) . (+2)) half

wanted :: Rational -> Bool
wanted r = let f = length . show
               n = f $ Real.numerator r
               d = f $ Real.denominator r
           in n > d

solution = length . filter wanted $ take 1000 values

main = print solution
