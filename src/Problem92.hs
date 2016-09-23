module Problem92 (main) where
{--
  Task description:
  A number chain is created by continuously adding the square of the digits in a number to form a new number until it has been seen before.

  For example,

  44 → 32 → 13 → 10 → 1 → 1
  85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89

  Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop.
  What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.

  How many starting numbers below ten million will arrive at 89?
--}

import Data.Char (digitToInt)
import qualified Data.List as List

import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Maybe as Maybe

{- The given chain function -}
next :: Int -> Int
next = sum . fmap ((^2) . digitToInt) . show

{- | This will also kill the zeroes :) -}
norm :: Int -> Int
norm = read . List.sort . show

step :: Int -> Int
step = next . norm

range :: [Int]
range = [1..10000000]

startMap :: HashMap Int Int
startMap = HashMap.fromListWith (+) . zip (fmap next range) $ repeat 1

nextMap :: HashMap Int Int -> HashMap Int Int
nextMap = HashMap.fromListWith (+) . fmap go . HashMap.toList
  where
    go tuple@(x, y)
      | x == 1 = tuple
      | x == 89 = tuple
      | otherwise = (next x, y)

maps = iterate nextMap startMap

solutionMap = head $ dropWhile (\m -> HashMap.size m > 2) maps

solution = solutionMap HashMap.! 89

main = print solution
