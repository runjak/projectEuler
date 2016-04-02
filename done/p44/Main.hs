module Main where

import Control.Monad
import qualified Data.Maybe as Maybe

type N = Integer

pentagonal :: N -> N
pentagonal n = n*(3*n - 1) `div` 2

unPentagonal :: N -> Maybe N
unPentagonal n = let y = 2 * fromIntegral n
                     x = (sqrt (12*y + 1) + 1)/6
                 in if ceiling x == floor x
                    then Just $ floor x
                    else Nothing

isPentagonal :: N -> Bool
isPentagonal = Maybe.isJust . unPentagonal

pentagonals :: [N]
pentagonals = map pentagonal [1..]

pPairs :: [(N, N)]
pPairs = do
  (n, y) <- zip [1..] pentagonals
  let next  = pentagonal $ n + 1
      upper = takeWhile (< y)
      lower = dropWhile (>= (next - y))
  x <- upper . lower $ pentagonals
  return (x, y)

diffs :: [(N, N, N)]
diffs = do
  (x,y) <- pPairs
  guard $ isPentagonal $ x + y
  let d = y - x
  guard $ isPentagonal d
  return (d,x,y)

solution = let (s, _, _) = head diffs
           in s

main = print solution
