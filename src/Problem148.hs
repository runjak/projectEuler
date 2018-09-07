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

  Find the number of entries which are not divisible by 7 in the first one billion (10^9) rows of Pascal's triangle.
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

parts :: [[N]]
parts = do
  n <- nRange
  return . fmap (binomialCoefficient n) $ kRangeForN n

pRows :: [[N]]
pRows = [[1],[1,1]] <> fmap (\ps -> [1] <> ps <> [1]) parts

-- Visualization:
showNum :: N -> String
showNum x
  | x `mod` divisor == 0 = " "
  | otherwise = "#"

showRows :: [[N]] -> String
showRows = unlines . fmap (concatMap showNum)

{-
> putStrLn . showRows $ take 21 pRows
01 #
02 ##
03 ###
04 ####
05 #####
06 ######
07 #######
08 #      #
09 ##     ##
10 ###    ###
11 ####   ####
12 #####  #####
13 ###### ######
14 ##############
15 #      #      #
16 ##     ##     ##
17 ###    ###    ###
18 ####   ####   ####
19 #####  #####  #####
10 ###### ###### ######
21 #####################

We start counding lines with 1 so that width and number of line coincide.
-}

-- Testing:
expecteds :: [N]
expecteds = fmap (fromIntegral . length . filter (== "#") . fmap showNum) pRows

testNumbers :: [N] -> [Maybe (N, N)]
testNumbers = zipWith (\e n -> if e == n then Nothing else Just (e, n)) expecteds

-- Experimenting:
triangleSizes :: [N]
triangleSizes = reverse . takeWhile (<= magic) $ iterate (* divisor) divisor

undivisibles :: N -> N
undivisibles n =
  let tSizes = dropWhile (> n) triangleSizes
  in go tSizes n
  where
    go :: [N] -> N -> N
    go []     n = n
    go (t:ts) n =
      let (d, m) = n `divMod` t
          y = (d + 1) * go ts m
      in if y == 0 then n else y

basePattern :: [N]
basePattern = cycle [1..divisor]

skipPattern :: N -> [N]
skipPattern n = concatMap (replicate $ fromIntegral n) basePattern
-- skipPattern n = cycle  [x * y | x <-[1..divisor], y <- [1..n]]
-- skipPattern n = cycle $ replicate (fromIntegral n) 1 <> [x * y | x <-[1..divisor], y <- [1..n]]

testPattern = fmap product $ List.transpose $ [basePattern] <> fmap skipPattern triangleSizes

chainPattern :: N -> [N] -> [N]
chainPattern n chain = cycle $ take (fromIntegral n) $ product <$> List.transpose [chain, skipPattern n]

sums :: [N] -> [N]
sums = go 0
  where
    go y []     = []
    go y (x:xs) =
      let y' = x + y
      in y' : go y' xs

{-
  sums $ take 10 basePattern
  fmap baseSum [0..9]
  -> first line is 0
  sum [1..7] == baseSum 6
-}
baseSum :: N -> N
baseSum n =
  let (d, m) = n `divMod` divisor
      m' = baseSums !! fromIntegral m
  in d * lastSum + m'
  where
    baseSums = sums [1..divisor]
    lastSum = last baseSums

{-
  We expect that:
  fmap (chainSum 7 baseSum) [0..6] == fmap baseSum [0..6]
  Further we expect behaviour similar to `sums testPattern` - that is:
  * (sums $ take 200 expecteds) == (fmap combinedSum [0..199])
  * (sums $ take 500 expecteds) == (fmap combinedSum [0..499])
  * (sums $ take 1000 expecteds) == (fmap combinedSum [0..999])
-}
chainSum :: N -> (N -> N) -> N -> N
chainSum s λ n =
  let (d, m) = n `divMod` s
      completedParts = baseSum (d - 1)
      completedSum = completedParts * λ (s - 1)
      startedParts = subtract completedParts $ baseSum d
      startedSum = startedParts * λ m
  in completedSum + startedSum

combinedSum :: N -> N
combinedSum = foldr chainSum baseSum triangleSizes

main = print $ combinedSum $ magic - 1