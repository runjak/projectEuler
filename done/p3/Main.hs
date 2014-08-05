module Main where

-- Give me some primes ,)
primes :: [ Integer ]
primes = sieve [2..]
-- -> But give them a little performant :P
sieve :: [Integer]->[Integer]
sieve (p:xs) = p:sieve [ x|x<-xs, x `mod` p /= 0 ]

-- The smallest primefactor will help getting the biggest primefactor
findSmallFactor :: Integer -> [Integer] -> Integer
findSmallFactor x [] = 0
findSmallFactor 0 (x:xs) = 0
findSmallFactor x (y:ys) = if (mod x y) == 0
	then y
	else findSmallFactor x ys

-- Here we jump a little more :P
getBigFactor :: Integer -> Integer -> Integer
getBigFactor 0 x = 0
getBigFactor x 0 = 0
getBigFactor x y = toInteger (floor (fromRational (toRational(x) / toRational(y))))

magicNumber :: Integer
magicNumber = 600851475143

primeFactors :: Integer -> [Integer]
primeFactors 0 = []
primeFactors 1 = []
primeFactors x = (if (findSmallFactor x primes) /= 1
	then [(findSmallFactor x primes)]
	else []) ++ primeFactors (getBigFactor x (findSmallFactor x primes))

findBiggest :: [Integer] -> Integer
findBiggest [] = 0
findBiggest (x:xs) = if x > findBiggest xs then x else findBiggest xs

problem3 :: Integer
problem3 = findBiggest (primeFactors magicNumber)

main = print problem3
