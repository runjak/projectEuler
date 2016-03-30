module Main where

import Data.Function (on)
import qualified Data.List as List

type N = Int

primes :: [N]
primes = 2 : 3 : sieve [] (tail primes) 3
  where
  notDivsBy d n     = n `rem` d /= 0
  sieve ds (p:ps) x = foldr (filter . notDivsBy) [x+2, x+4..p*p-2] ds
                   ++ sieve (p:ds) ps (p*p)

root :: N -> N
root = round . sqrt . fromIntegral

isPrime :: N -> Bool
isPrime x
  | x <= 1 = False
  | otherwise = notElem 0 . map (mod x) $ takeWhile (<= root x) primes

range :: [N]
range = [-1000,-999..1000]

type A = N
type B = N
newtype QForm = QForm (A,B)

instance Show QForm where
  show (QForm (a, b)) = "n²+"++ show a ++"n+"++ show b

calc :: QForm -> Int -> Int
calc (QForm (a,b)) n = n^2 + a*n + b

as :: [N]
as = range

{-|
  The b parameter of a QForm is not allowed to be <=1,
  because numbers <= 1 can't be prime
  and we're starting with n=0.
|-}
bs :: [N]
bs = filter (> 1) range

qForms :: [QForm]
qForms = do
  a <- as
  b <- bs
  return $ QForm (a,b)

score :: QForm -> N
score f = length $ takeWhile isPrime $ map (calc f) [0..]

wantedQForm :: QForm
wantedQForm = let qs = zip qForms $ map score qForms
                  q  = List.maximumBy (compare `on` snd) qs
              in fst q

solution = solution' wantedQForm
  where
    solution' :: QForm -> N
    solution' (QForm (a,b)) = a * b

main = print solution
