module Main where

import Data.Set (Set)
import qualified Data.Set as Set

type N = Int

fac :: N -> N
fac 0 = 1
fac n = n * fac (n - 1)

facs = map fac [0..9]

digits :: N -> [N]
digits = map (read . return) . show

facSum :: N -> N
facSum = sum . map (facs !!) . digits

predicate :: N -> Bool
predicate n = facSum n == n

combinations :: N -> Set N
combinations 0 = Set.fromList $ do
  x <- facs
  y <- facs
  return $ x+y
combinations n =
  let s1 = combinations $ n - 1
      s2 = Set.fromList $ do
             x <- facs
             y <- Set.toList s1
             return $ x+y
  in Set.union s1 s2

{-
  An upper range is given by:
  * 9999999 as fac 9 * 7 < 9999999
  …but I should opt to construct the searchspace
  in a manner of combining 2-7 different facs so that
  the resulting searchspace would be smaller.
-}
searchSpace = filter (>= 10) $ Set.toList $ combinations 6

main = print . sum $ filter predicate searchspace
