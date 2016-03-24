module Main where

import Control.Monad (guard)
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

type N = Int

upperLimit :: N
upperLimit = 28123

range = [1..upperLimit]

root :: N -> N
root = round . sqrt . fromIntegral

divisors :: N -> [N]
divisors x = nub $ 1 : do
  p <- [2..(root x)]
  guard $ (x`mod`p) == 0
  [p,x`div`p]

abundant :: N -> Bool
abundant x = (x <) . sum $ divisors x

type Abundant  = N
type Nabundant = N
ans :: ([Abundant],[Nabundant])
ans = partition abundant range

abundants  = fst ans
nabundants = snd ans

{-|
  aSums holds all numbers below upperLimit
  that are the sum of two abundants.
|-}
aSums :: Set N
aSums = Set.fromList $ do
  x <- abundants
  y <- abundants
  let s = x+y
  guard $ s <= upperLimit
  return s

{-|
  nSums holds all numbers below upperLimit
  that are not found in aSums.
|-}
nSums :: [N]
nSums = filter (not . flip Set.member aSums) range

main = print $ sum nSums
