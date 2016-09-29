module Problem90(main)where
{--
  Task description:
  Each of the six faces on a cube has a different digit (0 to 9) written on it;
  the same is done to a second cube.
  By placing the two cubes side-by-side in different positions we can form a variety of 2-digit numbers.

  For example, the square number 64 could be formed:
    ___    ___
   /__/|  /__/|
  |   || |   ||
  | 6 || | 4 ||
  |___|/ |___|/

  In fact, by carefully choosing the digits on both cubes it is possible to display
  all of the square numbers below one-hundred: 01, 04, 09, 16, 25, 36, 49, 64, and 81.

  For example, one way this can be achieved is by placing {0, 5, 6, 7, 8, 9}
  on one cube and {1, 2, 3, 4, 8, 9} on the other cube.

  However, for this problem we shall allow the 6 or 9 to be turned upside-down
  so that an arrangement like {0, 5, 6, 7, 8, 9} and {1, 2, 3, 4, 6, 7}
  allows for all nine square numbers to be displayed; otherwise it would be impossible to obtain 09.

  In determining a distinct arrangement we are interested in the digits on each cube, not the order.

  {1, 2, 3, 4, 5, 6} is equivalent to {3, 6, 4, 1, 2, 5}
  {1, 2, 3, 4, 5, 6} is distinct from {1, 2, 3, 4, 5, 9}

  But because we are allowing 6 and 9 to be reversed,
  the two distinct sets in the last example both represent the extended set {1, 2, 3, 4, 5, 6, 9}
  for the purpose of forming 2-digit numbers.

  How many distinct arrangements of the two cubes allow for all of the square numbers to be displayed?
--}

import Data.Function (on)
import qualified Data.List as List

type N = Int

{-
  I'm encoding the squares as tuples of digits.
  From these I want to generate two sets,
  where each tuple implies, that if one of its digits is in one set,
  the other digit must be in the other set.
-}
squares :: [(N,N)]
squares = [(0,1),(0,4),(0,9),(1,6),(2,5),(3,6),(4,9),(6,4),(8,1)]

simplifyDigit :: N -> N
simplifyDigit 9 = 6
simplifyDigit x = x

baseSets :: [([N],[N])]
baseSets = filter possibleDice . List.nub $
           sanitize <$> foldl go [([],[])] squares
  where
    go :: [([N],[N])] -> (N,N) -> [([N],[N])]
    go setStash (d1,d2) = do
      let d1' = simplifyDigit d1
          d2' = simplifyDigit d2
      (s1,s2) <- setStash
      [(d1':s1,d2':s2),(d2':s1,d1':s2)]

    sanitize :: ([N],[N]) -> ([N],[N])
    sanitize (xs, ys) = let f = List.nub . List.sort
                        in (f xs, f ys)

    possibleDice :: ([N],[N]) -> Bool
    possibleDice (xs, ys) = let f zs = length zs <= 6
                            in f xs && f ys && xs < ys

setWeight :: [N] -> N
setWeight xs = let len = length xs
                   doubleIfSix = if 6 `elem` xs then 2 else 1
                   emptyFields = 6 - len
               in if len == 6
                  then doubleIfSix
                  else 10^emptyFields * doubleIfSix

pairWeight :: ([N],[N]) -> N
pairWeight = uncurry ((*) `on` setWeight)

solution = sum $ fmap pairWeight baseSets

main = print solution