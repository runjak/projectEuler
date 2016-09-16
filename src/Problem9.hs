module Problem9 where
{--
  Task description:
  A Pythagorean triplet is a set of three natural numbers, a  < b  < c, for which,
  a^(2) + b^(2) = c^(2)

  For example, 3^(2) + 4^(2) = 9 + 16 = 25 = 5^(2).

  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product abc.
--}

import Control.Monad (guard)

triplets :: [(Integer,Integer,Integer)]
triplets = do
  z <- [1..]
  x <- [1..z]
  y <- [x..z]
  guard (x^2 + y^2 == z^2)
  return (x, y, z)

correct :: (Integer, Integer, Integer) -> Bool
correct (x,y,z) = x+y+z == 1000

merge :: (Integer,Integer,Integer) -> Integer
merge (x,y,z) = x*y*z

solve = merge . head $ filter correct triplets

main = print solve
