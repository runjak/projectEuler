module Problem28 where
{--
  Task description:
  Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:

  21 22 23 24 25
  20  7  8  9 10
  19  6  1  2 11
  18  5  4  3 12
  17 16 15 14 13

  It can be verified that the sum of the numbers on the diagonals is 101.

  What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
--}

ring :: Int -> [Int]
ring 1 = [1]
ring x = take 4 $ iterate (subtract (x - 1)) (x^2)

rings :: [Int]
rings = ring =<< [1, 3..1001]

main = print $ sum rings
