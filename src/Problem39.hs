module Problem39 where
{--
  Task description:
  If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.

  {20,48,52}, {24,45,51}, {30,40,50}

  For which value of p ≤ 1000, is the number of solutions maximised?
--}

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
perimeters = fmap perimeter validPts

solution :: Int
solution = snd . maximum . fmap (length &&& head) . group $ sort perimeters

main = print solution
