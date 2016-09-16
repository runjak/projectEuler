module Problem5 where
{--
Task description:
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest number that is evenly divisible by all of the numbers from 1 to 20?
--}

-- We need some primes to accomplish this one efficiently
primes :: [Integer]
primes = sieve [2..]
sieve :: [Integer]->[Integer]
sieve (p:xs) = p:sieve [ x|x<-xs, x `mod` p /= 0 ]

-- Gives all primes smaller then first argument
-- smallerThanValue, primes, appendix-list
smallerPrimes :: Integer -> [Integer] -> [Integer] -> [Integer]
smallerPrimes x [] z = z --Will we ever run out of primes o.O?
smallerPrimes x y z = if head y < x then head y : smallerPrimes x (tail y) z else []

--Es wird nicht das Produkt der Primzahlen sondern das Produkt der Primfaktoren aller Zahlen gebraucht.

-- Primes in range
primesInRange :: [Integer]
primesInRange = smallerPrimes 20 primes []

contains :: Integer -> [Integer] -> Bool
contains x = foldr (\ y -> (||) (y == x)) False

notPrimes :: [Integer] -> [Integer] -> [Integer]
notPrimes []_ = []
notPrimes (x:xs) y = [x | x<-xs, not(contains x y)]

notPrimesInRange :: [Integer]
notPrimesInRange = notPrimes [1..20] primesInRange

listDivision :: Integer -> [Integer] -> [Integer]
listDivision x = fmap (\ y -> toInteger (floor (fromRational (toRational y / toRational x))))


getPrimeFactor :: Integer -> [Integer] -> Integer
getPrimeFactor x [] = 0
getPrimeFactor x (y:ys) = if (mod x y == 0) && (x /= y) then y else getPrimeFactor x ys

getFactorList :: [Integer] -> [Integer]
getFactorList [] = []
getFactorList (x:xs) = if getPrimeFactor x primesInRange == 0 --We have a prime here, sire :P
  then x : getFactorList xs
  else getPrimeFactor x primesInRange : getFactorList(
    listDivision (getPrimeFactor x primesInRange)[x]
    `mappend`
      notPrimes
        (listDivision (getPrimeFactor x primesInRange) xs)
        [getPrimeFactor x primesInRange]
  )

problem5 :: [Integer]
problem5 = getFactorList notPrimesInRange `mappend` notPrimes primesInRange (getFactorList notPrimesInRange)

main = print problem5
