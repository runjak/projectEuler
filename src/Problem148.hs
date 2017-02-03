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

import Data.Monoid ((<>))
import Data.Ratio ((%))
import qualified Data.List as List
import qualified Data.Ratio as Ratio


type N = Integer

magic :: N
magic = 10^9

divisor :: N
divisor = 7

pows :: [N]
pows = tail $ iterate (*divisor) 1

isPow :: N -> Bool
isPow n = or $ (== n) <$> takeWhile (<= n) pows

gSum :: N -> N
gSum x = (x^2 + x) `div` 2

triangle :: [[N]]
triangle = iterate nextRow [1]
  where
    nextRow :: [N] -> [N]
    nextRow xs = 1 : (zipWith (+) xs $ tail xs) <> [1]

modTriangle :: [[N]]
modTriangle = fmap (fmap (`mod` divisor)) triangle

strTriangle :: [String]
strTriangle = fmap (fmap go) modTriangle
  where
    go 0 = '#'
    go _ = ' '

lineSums :: [N]
lineSums = fmap (fromIntegral . length . filter (=='#')) strTriangle

slowCount :: N -> N
slowCount n = let remove = sum $ take (fromIntegral n) lineSums
                  total = gSum n
              in total - remove

powCount :: N -> N
powCount n
  | n <= divisor = gSum n
  | isPow n = 28 * powCount (n `div` divisor)
  | otherwise = -1 -- Error case

test :: [Bool]  -- testing same results for slowCount and powCount
test = let sCounts = fmap slowCount pows
           pCounts = fmap powCount pows
       in zipWith (==) sCounts pCounts

tLineSums :: N -> [N]
tLineSums till = let ps = takeWhile (<= till) pows
                     pLSums = fmap calcPowLineSums ps
                 in take (fromIntegral till) $ merge pLSums
                 where
                   merge :: [[N]] -> [N]
                   merge (xs:ys:zss) = let zs = zipWith (+) xs ys
                                       in  merge (zs:zss)
                   merge [xs] = xs
                   merge [] = []

calcPowLineSums :: N -> [N]
calcPowLineSums p = let p' = p - 1
                        pre = replicate (fromIntegral p) 0
                        run = take (fromIntegral p) $ iterate (subtract 1) p'
                        runs = [fmap (*x) run | x <- [1..(divisor - 1)]]
                    in pre <> (go 1 (concat runs))
                    where
                      go :: N -> [N] -> [N]
                      go 1 xs = xs <> go 2 xs
                      go y xs = fmap (*y) xs <> go (y + 1) xs

diagonalLengthFor :: N -> [N]
diagonalLengthFor n = [n, n-1..1]

fish = List.transpose $ take 20 triangle

fSums = fmap go fish
  where
    go xs = zipWith subtract xs $ tail xs

-- solutionFor :: N -> N
solutionFor n = let upperPow = head $ dropWhile (< n) pows
                    upperCount = powCount upperPow
                    upperFields = gSum upperPow
                    magicFields = gSum n
                    wantedRatio = upperCount % upperFields
                in wantedRatio * (fromIntegral magicFields)
                -- in round $ fieldRatio * (fromIntegral upperCount)

{-
solution :: N
solution = solutionFor magic

main = print solution
-}

fac :: N -> N
fac 0 = 1
fac 1 = 1
fac n = product [1..n]

binomialCoefficient :: N -> N -> N
binomialCoefficient n k = fac n `div` (fac k * fac (n - k))

nRange :: [N]
nRange = [2 .. magic - 1]

kRangeForN :: N -> [N]
kRangeForN n = [1 .. n - 1]

parts = do
  n <- nRange
  return . fmap (binomialCoefficient n) $ kRangeForN n
