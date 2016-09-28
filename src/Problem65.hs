module Problem65(main) where
{-
  Task description:
  https://projecteuler.net/problem=65
-}

import           Data.Ratio ((%))
import qualified Data.Ratio as Ratio

ePartStream :: [Integer]
ePartStream = 2 : (go =<< [2, 4 ..])
  where
    go x = [1, x, 1]

approximate :: [Integer] -> Rational
approximate = foldr go 0
  where
    go :: Integer -> Rational -> Rational
    go x 0 = toRational x
    go x y = (toRational x) + (1 / y)

numeratorDigitSum :: Rational -> Integer
numeratorDigitSum = sum . fmap (read . return) . show . Ratio.numerator

solution = numeratorDigitSum . approximate $ take 100 ePartStream

main = print solution
