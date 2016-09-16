module Problem58 where
{--
  Task description:
  Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.

  37 36 35 34 33 32 31
  38 17 16 15 14 13 30
  39 18  5  4  3 12 29
  40 19  6  1  2 11 28
  41 20  7  8  9 10 27
  42 21 22 23 24 25 26
  43 44 45 46 47 48 49

  It is interesting to note that the odd squares lie along the bottom right diagonal,
  but what is more interesting is that 8 out of the 13 numbers lying along both diagonals are prime;
  that is, a ratio of 8/13 ≈ 62%.

  If one complete new layer is wrapped around the spiral above,
  a square spiral with side length 9 will be formed.
  If this process is continued, what is the side length of the square spiral for which the ratio of primes along both diagonals first falls below 10%?
--}

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
                   `mappend` sieve (p:ds) ps (p*p)

root :: N -> N
root = round . sqrt . fromIntegral

isPrime :: N -> Bool
isPrime x
  | x <= 1 = False
  | otherwise = notElem 0 . fmap (mod x) $ takeWhile (<= root x) primes

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
          ps = take 3 . tail $ iterate (+q) start
          pps = points' (start+dist) dists
      in [start] `mappend` ps `mappend` pps

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
