module Problem32 where
{--
  Task description:
  We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once;
  for example, the 5-digit number, 15234, is 1 through 5 pandigital.

  The product 7254 is unusual, as the identity, 39 × 186 = 7254,
  containing multiplicand, multiplier, and product is 1 through 9 pandigital.

  Find the sum of all products whose multiplicand/multiplier/product
  identity can be written as a 1 through 9 pandigital.

  HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.
--}

import Control.Monad
import Data.Char (digitToInt)
import qualified Data.List as List
import Data.Set (Set, (\\))
import qualified Data.Set as Set

digits :: String
digits = "123456789"

charSet :: Set Char
charSet = Set.fromList digits

type N = Int

-- | test if `x,y` form a pandigital product over `digits`.
predicate :: N -> N -> Bool
predicate x y = let s = show =<< [x,y,x*y]
                in digits == List.sort s

startSet :: Set N
startSet = Set.fromList $ fmap digitToInt digits

{-|
  Generates a set containing all numbers that follow these criteria:
  * Initial set of numbers is given as a `Set N`.
  * Maximum length of numbers will be `maxLen` when initial length was 1.
  * Only chars from `baseSet` will be used.
  * No number will have duplicate digits.
|-}
numbersForSet :: N -> Set Char -> Set N -> Set N
numbersForSet maxLen baseSet = foldl1 Set.union . take maxLen . iterate extend
  where
    extend :: Set N -> Set N
    extend s = Set.fromList $ do
      n <- Set.toList s
      let nDigits = Set.fromList $ show n
      d <- Set.toList $ baseSet \\ nDigits
      return . read $ show n `mappend` [d]

factors :: [(N, N)]
factors = do
  -- The first factor cannot be longer than 4 digits.
  x <- Set.toList $ numbersForSet 4 charSet startSet
  let xDigits = Set.fromList $ show x
  {-
    For each second factor, the following must hold:
    * Digits must be from `charSet \\ xDigits`.
    * Length must be `<= Set.size xDigits`.
    * Must be smaller than `x` to avoid finding duplicates.
  -}
  let yDigits      = charSet \\ xDigits
      yMaxLen      = Set.size charSet - Set.size xDigits
      yStartSet    = Set.map digitToInt yDigits
      ySet         = numbersForSet yMaxLen yDigits yStartSet
      ySmallerXSet = fst $ Set.split x ySet
  y <- Set.toList ySmallerXSet
  -- Making sure we satisfy the predicate:
  guard $ predicate x y
  -- Returning the pairs:
  return (x, y)

solution = sum . List.nub $ fmap (uncurry (*)) factors

main = print solution
