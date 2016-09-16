module Problem52 where
{--
  Task description:
  It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.

  Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.
--}

import Data.List (sort)

sameDigits x y  = (==)(sort $ show x) (sort $ show y)

allSame :: [Int] -> Bool
allSame (x:xs) = all (sameDigits x) xs
allSame _ = False

vals = [fmap (x*) [2..6] | x <- [1..]]

results = flip zip [1..] $ fmap allSame vals

solution = snd . head $ filter fst results

main = print solution
