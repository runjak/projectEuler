module Main where

import Control.Monad (guard)
import Data.List
import Data.Set (Set)
import qualified Data.Set as S

upperLimit :: Int
upperLimit = 28123

range = [1..upperLimit]

root :: Int -> Int
root = round . sqrt . fromIntegral

divisors :: Int -> [Int]
divisors x = nub $ 1 : do
	p <- [2..(root x)]
	guard $ (x`mod`p) == 0
	[p,x`div`p]

abundant :: Int -> Bool
abundant x = (x <) . sum $ divisors x

type Abundant  = Int
type Nabundant = Int
ans :: ([Abundant],[Nabundant])
ans = partition abundant range

abundants  = fst ans
nabundants = snd ans

isSum :: Int -> Bool
isSum x = False

nSums :: [Int]
nSums = filter (not . isSum)

main = print ans
