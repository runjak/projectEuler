module Problem26 where
{--
  Task description:
  A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

      1/2	= 	0.5
      1/3	= 	0.(3)
      1/4	= 	0.25
      1/5	= 	0.2
      1/6	= 	0.1(6)
      1/7	= 	0.(142857)
      1/8	= 	0.125
      1/9	= 	0.(1)
      1/10	= 	0.1

  Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

  Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
--}

import Data.Function (on)
import Data.List (maximumBy)
import Data.Set(Set)
import qualified Data.Set as S

type Divisor = Int
range :: [Divisor]
range = [2..999]

digits :: Int -> Divisor -> [(Int,Divisor)]
digits 0 _ = []
digits x y
  | x < y = digits (x * 10) y
  | otherwise = (x,y):digits (x`mod`y) y

findCircle :: Set (Int,Divisor) -> [(Int,Divisor)] -> Int
findCircle _ [] = 0
findCircle s (x:xs)
  | S.member x s = (+1) . length . fst $ break (==x) xs
  | otherwise = findCircle (S.insert x s) xs

compares :: [(Divisor,Int)]
compares = fmap (\r -> (r, findCircle S.empty $ digits 1 r)) range

target = maximumBy (compare `on` snd) compares

main = print $ fst target
