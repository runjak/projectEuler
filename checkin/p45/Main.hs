module Main where

import Control.Arrow as Arrow
import Data.List     as List

type N      = Int
type Stream = ([N],[N])
type Point  = (N,N)

-- Definition of number functions:
triangle,pentagonal,hexagonal :: N -> N
triangle   n = n*(n+1)   `div` 2
pentagonal n = n*(3*n-1) `div` 2
hexagonal  n = n*(2*n-1) -- Evry hexagonal number is also a triangle number.

-- n = (sqrt(24x+1)+1)/6
isPentagonal :: N -> Bool
isPentagonal n = let m = 24*n+1
                     o = sqrt $ fromIntegral m :: Double
                     p = (o+1)/6
                 in ceiling p == floor p

solution = last . take 3 . filter isPentagonal $ map hexagonal [1..]

main = print solution
