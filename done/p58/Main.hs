module Main where

import GHC.Real ((%),Ratio ((:%)))
import qualified GHC.Real as Real

import Data.Set (Set)
import qualified Data.Set as Set

type N = Integer
type R = Rational

primes :: [N]
primes = 2 : 3 : sieve [] (tail primes) 3
  where
  notDivsBy d n     = n `rem` d /= 0
  sieve ds (p:ps) x = foldr (filter . notDivsBy) [x+2, x+4..p*p-2] ds
                   ++ sieve (p:ds) ps (p*p)

root :: N -> N
root = round . sqrt . fromIntegral

isPrime :: N -> Bool
isPrime x
  | x <= 1 = False
  | otherwise = notElem 0 . map (mod x) $ takeWhile (<= root x) primes

-- | 1,9,25,…
squares :: [N]
squares = [x*x|x<-[1,3..]]

-- | 8,16,24,…
sqDists :: [N]
sqDists = zipWith subtract squares $ tail squares

-- | 1,3,5,7,9,13,17,21,25,31,37,43,49,…
points :: [N]
points = points' (head squares) sqDists
  where
    points' :: N -> [N] -> [N]
    points' start (dist:dists) =
      let q  = dist `div` 4
          ps = take 3 $ tail $ iterate (+q) start
          pps = points' (start+dist) dists
      in [start]++ps++pps

primeRatios :: [(N,R)]
primeRatios = go 0 0 points
  where
    go :: N -> N -> [N] -> [(N,R)]
    go nPrimes nTotal (n:ns)
      | isPrime n =
          let nPrimes' = nPrimes + 1
              nTotal'  = nTotal + 1
          in (n, nPrimes' % nTotal') : go nPrimes' nTotal' ns
      | otherwise =
          let nTotal' = nTotal + 1
          in (n, nPrimes % nTotal') : go nPrimes nTotal' ns

predicate :: (N,R) -> Bool
predicate = (< 1%10) . snd

wantedCorners :: [(N,R)]
wantedCorners = go $ tail primeRatios
  where
    go :: [(N,R)] -> [(N,R)]
    go ns = let (corners,ns') = splitAt 4 ns
                good = any predicate corners
            in if good then corners else go ns'

solution = root . fst . last $ wantedCorners

main = print solution
