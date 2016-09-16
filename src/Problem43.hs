module Problem43 where
{--
  Task description:
  The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order,
  but it also has a rather interesting sub-string divisibility property.

  Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:

  d2d3d4=406 is divisible by 2
  d3d4d5=063 is divisible by 3
  d4d5d6=635 is divisible by 5
  d5d6d7=357 is divisible by 7
  d6d7d8=572 is divisible by 11
  d7d8d9=728 is divisible by 13
  d8d9d10=289 is divisible by 17
  Find the sum of all 0 to 9 pandigital numbers with this property.
--}

import Control.Monad
import Control.Monad.Writer (Sum)
import Data.List
import Data.Monoid

import qualified Control.Monad.Writer as Writer

digits = [0..9] :: [Int]

mkTriplets :: [Int] -> [(Int,Int,Int)]
mkTriplets (a:xs@(b:c:_)) = (a,b,c):mkTriplets xs
mkTriplets _ = []

triplets :: [[(Int, Int, Int)]]
triplets = mkTriplets <$> permutations digits

fromTriplets :: [(Int,Int,Int)] -> [Int]
fromTriplets ((a,b,c):xs)
  | null xs   = a:b:[c]
  | otherwise = a:fromTriplets xs

toNumber :: [Int] -> Int
toNumber = foldl1 (\a b -> a*10+b)

chkNums = [2,3,5,7,11,13,17] :: [Int]

chk :: Int -> (Int,Int,Int) -> Bool
chk n (a,b,c) = (a*100 + b*10 + c) `rem` n == 0

chks = fmap chk chkNums

pass = and . zipWith ($) chks . tail

single :: [(Int,Int,Int)] -> Sum Int
single t
  | pass t    = Sum . toNumber $ fromTriplets t
  | otherwise = Sum 0

mySum = foldl1 (<>) $ fmap single triplets

main :: IO ()
main = print $ getSum mySum
