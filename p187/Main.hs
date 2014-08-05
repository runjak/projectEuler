module Main where

import Control.Monad

magic :: Int
magic = 10^7

range :: [Int]
range = [4..magic] -- | 4 is the first semiprime

root :: Int -> Int
root = round . sqrt . fromIntegral

primesTill :: Int -> [Int]
primesTill x
	| x < 30 = takeWhile (<=x) [2,3,5,7,11,13,17,19,23,29]
	| otherwise = do
		let r = root x
		let primes = primesTill r
		let newPrimes = do
			test <- [r..x]
			guard . all (/=0) $ map (mod test) primes
			return test
		primes++newPrimes

primes :: [Int]
primes = primesTill magic

primesTillRoot :: [Int]
primesTillRoot = primesTill $ root magic

isPrime :: Int -> Bool
isPrime x = all (/=0) . map (mod x) $ takeWhile (<=(root x)) primes

semiprimes :: [Int]
semiprimes = do
	sp <- range
	t <- takeWhile (< root sp) primesTillRoot
	guard $ t < sp
	guard $ sp `mod` t == 0
	guard $ isPrime (sp`div`t)
	return sp

main = print $ length semiprimes
