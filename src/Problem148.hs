module Problem148 where
{--
  Task description:
  We can easily verify that none of the entries in the first seven rows of Pascal's triangle are divisible by 7:
   	 	 	 	 	 	 1
   	 	 	 	 	 1	 	 1
   	 	 	 	 1	 	 2	 	 1
   	 	 	 1	 	 3	 	 3	 	 1
   	 	 1	 	 4	 	 6	 	 4	 	 1
   	 1	 	 5	 	10	 	10	 	 5	 	 1
  1	 	 6	 	15	 	20	 	15	 	 6	 	 1

  However, if we check the first one hundred rows, we will find that only 2361 of the 5050 entries are not divisible by 7.

  Find the number of entries which are not divisible by 7 in the first one billion (10^(9)) rows of Pascal's triangle.
--}

import Data.Maybe as M

magic :: Integer
magic = 10^9
magicT = TS 1 magic
divisor :: Integer
divisor = 7
pows = tail $ iterate (*divisor) 1

data TriangleStump = TS {
      low :: Integer
    , up :: Integer
  } deriving (Show)

gSum :: Integer -> Integer
gSum x = (x^2 + x) `div` 2

size :: TriangleStump -> Integer
size ts = (gSum $ up ts) - (gSum . subtract 1 $ low ts)
