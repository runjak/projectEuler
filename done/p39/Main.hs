module Main where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.List (sort, group)

type Triangle = (Int,Int,Int)

pytrips :: [Triangle]
pytrips = do
  z <- [1..]
  x <- [1..z]
  y <- [x..z]
  guard (x^2 + y^2 == z^2)
  return (x, y, z)

perimeter :: Triangle -> Int
perimeter (a,b,c) = a+b+c

validPts :: [Triangle]
validPts = takeWhile (\x -> perimeter x <= 1000) pytrips

perimeters :: [Int]
perimeters = map perimeter validPts

solution :: Int
solution = snd . maximum . map (length &&& head) . group $ sort perimeters

main = print solution
