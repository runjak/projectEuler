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

good :: Int -> Bool
good 89 = True
good _ = False

run :: [Int] -> (Int, [Int])
run = first length . partition good . filter (/= 1) . fmap step
