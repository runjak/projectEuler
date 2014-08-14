module Main where

import GHC.Real ((%),Ratio ((:%)))
import qualified GHC.Real as Real

type N = Integer
type R = Rational

spiral :: N -> N -> [(N,N,N,N)]
spiral start step = let [a,b,c,d] = take 4 . drop 1 $ iterate (+step) start
                    in (a,b,c,d):spiral d (step+2)

primes :: [N]
primes = 2: 3: sieve [] (tail primes) 3
	where
	notDivsBy d n     = n `rem` d /= 0
	sieve ds (p:ps) x = foldr (filter . notDivsBy) [x+2,x+4..p*p-2] ds
		                ++ sieve (p:ds) ps (p*p)

isPrime :: N -> Bool
isPrime n
  | n <= 1    = False
  | otherwise = let search = last . takeWhile (<= n) 
                in n == search primes

solution :: N
solution = go (0 :% 0, 0 :% 0) $ zip sizes $ spiral 1 2
  where
    go :: (R, R) -> [(N, (N,N,N,N))] -> N
    go (x, y) ((size, (a,b,c,d)):xs) = let pCount = toInteger . length . filter isPrime
                                           x'     = (pCount [a,c] + Real.numerator x) % (Real.denominator x + 2)
                                           y'     = (pCount [b,d] + Real.numerator y) % (Real.denominator y + 2)
                                           break  = x' < threshold && y' < threshold
                                       in if break then size else go (x', y') xs

    threshold = 1 % 10 :: R
    sizes = [3,5..] :: [N]

main = print solution
