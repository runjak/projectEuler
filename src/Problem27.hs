module Problem27 where
{--
  Task description:
  Euler published the remarkable quadratic formula:

  n² + n + 41

  It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39.
  However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41,
  and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.

  Using computers, the incredible formula  n² − 79n + 1601 was discovered,
  which produces 80 primes for the consecutive values n = 0 to 79.
  The product of the coefficients, −79 and 1601, is −126479.

  Considering quadratics of the form:

      n² + an + b, where |a| < 1000 and |b| < 1000

      where |n| is the modulus/absolute value of n
      e.g. |11| = 11 and |−4| = 4

  Find the product of the coefficients, a and b, for the quadratic expression that produces
  the maximum number of primes for consecutive values of n, starting with n = 0.
--}

import Data.Function (on)
import qualified Data.List as List

type N = Int

primes :: [N]
primes = 2 : 3 : sieve [] (tail primes) 3
  where
  notDivsBy d n     = n `rem` d /= 0
  sieve ds (p:ps) x = foldr (filter . notDivsBy) [x+2, x+4..p*p-2] ds
                   `mappend` sieve (p:ds) ps (p*p)

root :: N -> N
root = round . sqrt . fromIntegral

isPrime :: N -> Bool
isPrime x
  | x <= 1 = False
  | otherwise = notElem 0 . fmap (mod x) $ takeWhile (<= root x) primes

range :: [N]
range = [-1000,-999..1000]

type A = N
type B = N
newtype QForm = QForm (A,B)

instance Show QForm where
  show (QForm (a, b)) = concat ["n²+", show a, "n+", show b]

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
score f = length . takeWhile isPrime $ fmap (calc f) [0..]

wantedQForm :: QForm
wantedQForm = let qs = zip qForms $ fmap score qForms
                  q  = List.maximumBy (compare `on` snd) qs
              in fst q

solution = solution' wantedQForm
  where
    solution' :: QForm -> N
    solution' (QForm (a,b)) = a * b

main = print solution
