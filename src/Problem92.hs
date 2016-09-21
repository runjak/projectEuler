module Problem92 where
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

import Control.Arrow
import Data.Char (digitToInt)
import Data.List (sort, partition)
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
norm = read . sort . show

step :: Int -> Int
step = next . norm

range :: [Int]
range = [1..10000000]

group :: [Int] -> [(Int, Int)]
group = fmap go . List.group . sort . fmap norm
  where
    go :: [Int] -> (Int, Int)
    go xs = (head xs, length xs)

initialPairs :: [(Int, Int)]
initialPairs = group range

-- | A pair is a number and a count.
pairsToMap :: [(Int, Int)] -> HashMap Int Int
pairsToMap = foldl go HashMap.empty
  where
    go :: HashMap Int Int -> (Int, Int) -> HashMap Int Int
    go xToCountMap ( 1, count) = xToCountMap
    go xToCountMap (89, count) = HashMap.insertWith (+) 89 count xToCountMap
    go xToCountMap ( x, count) = HashMap.insertWith (+) (next x) (count + 1) xToCountMap

solve xs =
  let xToCountMap = pairsToMap xs
      lengthIsOne = null . tail $ HashMap.toList xToCountMap
  in if lengthIsOne
    then snd . head $ HashMap.toList xToCountMap
    else solve $ HashMap.toList xToCountMap

main = print $ solve initialPairs
