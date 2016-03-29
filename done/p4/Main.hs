module Main where

-- Use show to pass a converted String ,)
isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome x = if (head x)==(last x)
  then isPalindrome (tail (init x))
  else False

-- Creates possibly wanted palindromes
palindromeCreation :: [Integer]->[Integer]
palindromeCreation (x:xs) = [x|x<-xs, isPalindrome (show x)]

-- Creates the list of palindromes that need to be checked
palindromeList :: [Integer]
palindromeList = palindromeCreation [(999*999),((999*999)-1)..(100*100)]

-- Used to test, wether Integer would be a valid factor in this problem
validFactor :: Integer -> Bool
validFactor x = (length (show x)) == 3

-- Getting the bigger Factor
getBigFactor :: Integer -> Integer -> Integer
getBigFactor 0 x = 0
getBigFactor x 0 = 0
getBigFactor x y = toInteger (floor (fromRational (toRational(x) / toRational(y))))

-- Tests a palindrome for a list of factors and returns a factor and a validVactor as a tuple in a list
hasValidFactors :: Integer -> [Integer] -> Bool
hasValidFactors p[] = False
hasValidFactors p(x:xs) = (if (validFactor (getBigFactor p x))
  then True
  else hasValidFactors p xs)--Could perhaps've been a little smarter.. perhaps

-- Filter possible Factors
getFactors :: Integer -> [Integer] -> [Integer]
getFactors p (x:xs) = [x|x<-xs, (mod p x) == 0]

-- Finds the first palindrome with correct factors
wantedP :: [Integer] -> Integer
wantedP [] = 0
wantedP (p:ps) = if (hasValidFactors p(getFactors p [100..999]))
  then p
  else wantedP ps

problem4 :: Integer
problem4 = wantedP palindromeList
