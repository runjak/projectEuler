module Problem215 where
{-
  Task description:
  Consider the problem of building a wall out of 2×1 and 3×1 bricks (horizontal×vertical dimensions) such that,
  for extra strength, the gaps between horizontally-adjacent bricks never line up in consecutive layers, i.e. never form a "running crack".

  For example, the following 9×3 wall is not acceptable due to the running crack shown in red:

  There are eight ways of forming a crack-free 9×3 wall, written W(9,3) = 8.

  Calculate W(32,10).
-}

import Control.Monad
import Data.Function (on)
import Data.Graph (Graph, Vertex)
import Data.Matrix (Matrix)
import Data.Maybe (fromJust)
import qualified GHC.Arr as Arr
import qualified Data.Graph as Graph
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Matrix as Matrix

{-
A row may only ever depend on the preceding row.
We may have cycles of rows that are allowed to follow each other.
An interesting question will be wether collapsing such cycles will be easier than simply 'exploring them'.
In both ways memoization may come in handy here.
-}

type Brick = Int
type Line = [Brick]
type Width = Int
type Height = Int
type Wall = [Line]

bricks :: [Brick]
bricks = [2, 3]

buildLines :: Width -> [Line]
buildLines 0 = [[]]
buildLines w = do
  brick <- bricks
  guard $ brick <= w
  lineRest <- buildLines $ w - brick
  return $ brick:lineRest

matchingLines :: Line -> Line -> Bool
matchingLines l1 l2 = not $ (matchingCracks `on` crackPositions) l1 l2
  where
    crackPositions :: Line -> [Int]
    crackPositions = fmap sum . init . tail . List.inits

    matchingCracks :: [Int] -> [Int] -> Bool
    matchingCracks [] _ = False
    matchingCracks _ [] = False
    matchingCracks xss@(x:xs) yss@(y:ys)
      | x == y = True
      | x < y = matchingCracks xs yss
      | x > y = matchingCracks xss ys

{-
With the matchingLines predicate I can generate a graph out of the possible lines.
We're searching for all routes of a given length that traverse this graph
and that start in any of it's nodes.
-}

mkLineEdges :: [Line] -> [(Line, Line)]
mkLineEdges [] = []
mkLineEdges (l:ls) = findMatchingLines l ls `mappend` mkLineEdges ls
  where
    findMatchingLines :: Line -> [Line] -> [(Line, Line)]
    findMatchingLines l lines = concat $ do
      l' <- lines
      guard $ matchingLines l l'
      return [(l,l'), (l',l)]

mkLineToVertex :: [Line] -> (Line -> Vertex)
mkLineToVertex lines = let lvMap = Map.fromList $ zip lines [0..]
                       in fromJust . (`Map.lookup` lvMap)

graphForWidth :: Width -> Graph
graphForWidth w = let lines = buildLines w
                      lEdges = mkLineEdges lines
                      bounds = (0, length lines - 1)
                      toVertex = mkLineToVertex lines
                      edges = [(toVertex a, toVertex b)|(a,b) <- lEdges]
                  in Graph.buildG bounds edges

graphToAdjMatrix :: Graph -> Matrix Int
graphToAdjMatrix g =
  let (minBound, maxBound) = Arr.bounds g
  in Matrix.fromLists $ do
    (v, edges) <- Arr.assocs g
    let edgeSet = HashSet.fromList edges
        adjForVertex v = if v `HashSet.member` edgeSet then 1 else 0
    return [adjForVertex v|v <- [minBound..maxBound]]

adjMatrixForWidth :: Width -> Matrix Int
adjMatrixForWidth = graphToAdjMatrix . graphForWidth

{- How to find the possible walks in the Graph? -}
-- Something is wrong with countWalls - I should debug it on paper for the 9 3 case.
countWalls :: Width -> Height -> Int
countWalls w h = let g = graphForWidth w
                     m = graphToAdjMatrix g
                 in sum . Matrix.toList $ m^h
