module Problem57 where
{--
  Task description:
  It is possible to show that the square root of two can be expressed as an infinite continued fraction.

  √ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...

  By expanding this for the first four iterations, we get:

  1 + 1/2 = 3/2 = 1.5
  1 + 1/(2 + 1/2) = 7/5 = 1.4
  1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
  1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...

  The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985,
  is the first example where the number of digits in the numerator exceeds the number of digits in the denominator.

  In the first one-thousand expansions, how many fractions contain a numerator with more digits than denominator?
--}

import qualified GHC.Real as Real

half = 1 / 2 :: Rational

values = (+1) <$> iterate ((1/) . (+2)) half

wanted :: Rational -> Bool
wanted r = let f = length . show
               n = f $ Real.numerator r
               d = f $ Real.denominator r
           in n > d

solution = length . filter wanted $ take 1000 values

main = print solution
