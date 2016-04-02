module Main where

import Control.Monad
import qualified Data.Maybe as Maybe

type N = Int

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

diffs = do
  (x,y) <- pPairs
  guard $ isPentagonal $ x + y
  let d = y - x
  guard $ isPentagonal d
  return (d,x,y)

-- This approach is too slow at printing anything useful.
-- I shall rethink this and figure out something more elegant.

{-|
  Since we need the sum of two pentagonal numbers to be a pentagonal number as well,
  we can do the following:
    p(z) = p(x)+p(y)
         = (3z^2-z)/2 = (3x^2-x)/2 + (3y^2-y)/2
  ≡ 0 = (3x^2-x)/2 + (3y^2-y)/2 - (3z^2-z)/2
  ≡ 0 = 3x^2 + 3y^2 - 3z^2 - x - y + z
  Where we have x,y,z ∈ ℕ and z > x ≧ y
  Knowing this it should be possible to reduce the searchspace at least a bit.
|-}

main = print diffs
