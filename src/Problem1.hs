module Problem1 where
{--
Task description:
If we list all the natural numbers below 10 that are multiples of 3 or 5,
we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
--}

-- Used to create the counters needed by sumMultiples.
-- A counter has a max-value which is given, and starts at 0
counterCreation :: [Integer] -> [(Integer,Integer)]
counterCreation = fmap (\ x -> (0, x))

-- Used to increase the counting-value of a Counter, which is the first Value.
counterInc :: [(Integer,Integer)] -> [(Integer,Integer)]
counterInc [] = []
counterInc ((a,b):xs) = (a+1,b) : counterInc xs

-- Checks if one Counter was >= than it's maximum, and resets it to 0.
-- If any Counter was resettet, the returned Bool will be true.
counterCheck :: [(Integer,Integer)] -> (Bool,[(Integer,Integer)])
counterCheck [] = (False,[])
counterCheck [(a,b)] = (a >= b, if a >= b then [(0,b)] else [(a,b)])
counterCheck ((a,b):xs) = (fst (counterCheck xs) || a >= b,
  (if a >= b then (0,b) else (a,b)):snd (counterCheck xs))

-- Builds up a new List of Items with only those contained
-- where fst (counterCheck [(Integer,Integer)]) is True.
-- This means, that the Counters are used to find multiples,
-- and are thereby increased with each recursion.
filterMultiples :: [Integer] -> [(Integer,Integer)] -> [Integer]
filterMultiples [][] = []
filterMultiples []_ = []
filterMultiples (x:xs) c = [x | fst (counterCheck c)] `mappend` filterMultiples xs (counterInc (snd (counterCheck c)))

-- Let's get it solved ,)
problem1 :: Integer
problem1 = sum (filterMultiples [0..999] (counterCreation [3,5]))

main = print problem1
