module Problem96 where
{--
  Task description:
  Su Doku (Japanese meaning number place) is the name given to a popular puzzle concept. Its origin is unclear, but credit must be attributed to Leonhard Euler who invented a similar, and much more difficult, puzzle idea called Latin Squares. The objective of Su Doku puzzles, however, is to replace the blanks (or zeros) in a 9 by 9 grid in such that each row, column, and 3 by 3 box contains each of the digits 1 to 9. Below is an example of a typical starting puzzle grid and its solution grid.
  0 0 3
  9 0 0
  0 0 1	0 2 0
  3 0 5
  8 0 6	6 0 0
  0 0 1
  4 0 0
  0 0 8
  7 0 0
  0 0 6	1 0 2
  0 0 0
  7 0 8	9 0 0
  0 0 8
  2 0 0
  0 0 2
  8 0 0
  0 0 5	6 0 9
  2 0 3
  0 1 0	5 0 0
  0 0 9
  3 0 0


  4 8 3
  9 6 7
  2 5 1	9 2 1
  3 4 5
  8 7 6	6 5 7
  8 2 1
  4 9 3
  5 4 8
  7 2 9
  1 3 6	1 3 2
  5 6 4
  7 9 8	9 7 6
  1 3 8
  2 4 5
  3 7 2
  8 1 4
  6 9 5	6 8 9
  2 5 3
  4 1 7	5 1 4
  7 6 9
  3 8 2

  A well constructed Su Doku puzzle has a unique solution and can be solved by logic, although it may be necessary to employ "guess and test" methods in order to eliminate options (there is much contested opinion over this). The complexity of the search determines the difficulty of the puzzle; the example above is considered easy because it can be solved by straight forward direct deduction.

  The 6K text file, sudoku.txt (right click and 'Save Link/Target As...'), contains fifty different Su Doku puzzles ranging in difficulty, but all with unique solutions (the first puzzle in the file is the example above).

  By solving all fifty puzzles find the sum of the 3-digit numbers found in the top left corner of each solution grid; for example, 483 is the 3-digit number found in the top left corner of the solution grid above.
--}

import Data.List (nub,delete,(\\),minimumBy)
import Data.Maybe (fromMaybe)
import Data.Char (isDigit)
import qualified Data.Map as M
import Control.Monad (guard)
import System.IO (readFile)

{- ----- ----- ----- ----- ----- types and data ----- ----- ----- ----- ----- -}

type Point = (Int,Int)
type Element = [Point]

data SudokuTile = SudokuTile {
   possibleNumbers :: [Int]
  ,point           :: Point
  ,concluded       :: Bool -- | helps remembering which fields are already visited, default is False
} deriving (Show, Eq)

type Field = M.Map Point SudokuTile
type STF = (SudokuTile -> SudokuTile)

{- ----- ----- ----- ----- ----- usefull functions for data ----- ----- ----- ----- ----- -}

setField :: SudokuTile -> Int -> SudokuTile
setField tile x = tile{possibleNumbers=[x]}

setConclusion :: Bool -> SudokuTile -> SudokuTile
setConclusion b t = t{concluded=b}

disableField :: SudokuTile -> Int -> SudokuTile
disableField tile x = tile{possibleNumbers = delete x $ possibleNumbers tile}

maybefy :: (a -> a) -> Maybe a -> Maybe a
maybefy f Nothing = Nothing
maybefy f (Just a) = Just $ f a

alter :: STF -> Field -> Element -> Field
alter f = foldl $ alter1 f

alter1 :: STF -> Field -> Point -> Field
alter1 f = flip (M.alter (maybefy f))

points :: Element
points = [(x,y)|x<-[0..8],y<-[0..8]]

elements :: [Element]
elements = concat [blocks, cols, rows]
  where
    blocks = fmap (\x->x : getBlock x) [(x,y) | x<-[0,3,6], y<-[0,3,6]]
    cols = fmap (\x->x : getCol x) [(x,0) | x<-[0..8]]
    rows = fmap (\x->x : getRow x) [(0,y) | y<-[0..8]]

-- | excludes self
getRow :: Point -> Element
getRow (x,y) = [(x',y) | x'<-[0..8], x'/=x]

-- | excludes self
getCol :: Point -> Element
getCol (x,y) = [(x,y') | y'<-[0..8], y'/=y]

-- | excludes self
getBlock :: Point -> Element
getBlock (x,y) =
  let (a,b) = (x `div` 3 * 3, y `div` 3 * 3)
  in [(c,d) | c<-[a..(a+2)], d<-[b..(b+2)], (c,d)/=(x,y)]

-- | excludes self
getAffected :: Point -> Element
getAffected point = nub $ getBlock point `mappend` (getRow point `mappend` getCol point)

{- ----- ----- ----- ----- ----- solving algorithm ----- ----- ----- ----- ----- -}
-- | This is not the most elegant, but works :P
exhausive :: (Field -> Field) -> Field -> Field
exhausive f field =
  let next = f field
  in if field /= next
    then exhausive f next
    else field

{- --- --- --- Conclusion --- --- --- -}
conclusion :: Field -> Field
conclusion field = foldl conclude field $ M.elems field

conclude :: Field -> SudokuTile -> Field
conclude field tile -- | Improve: work on affected Fields
  | concludeAble tile =
    let newField = alter1 (setConclusion True) field $ point tile
    in alter (flip disableField . head $ possibleNumbers tile) newField (getAffected $ point tile)
  | otherwise = field

conclude' :: Field -> Point -> Field
conclude' field = maybe field (conclude field) . flip M.lookup field

concludeAble :: SudokuTile -> Bool
concludeAble x = not (concluded x) && ((==1) . length $ possibleNumbers x)

{- --- --- --- Exclusion --- --- --- -}
exclusion :: Field -> Field
exclusion = flip (foldl exclude) elements

-- | takes advantage of conclusion :P
exclude :: Field -> Element -> Field
exclude field e =
  let
    tiles = (maybe [] return . flip M.lookup field) =<< e :: [SudokuTile]
    set = [(x,delete x tiles)|x<-tiles] :: [(SudokuTile,[SudokuTile])]
  in
    foldl exclude' field set
  where
    exclude' :: Field -> (SudokuTile,[SudokuTile]) -> Field
    exclude' field (t,ts)
      | length (possibleNumbers t) <= 1 = field
      | otherwise = check field (point t) . foldl1 (\\) $ possibleNumbers t : fmap possibleNumbers ts

    check :: Field -> Point -> [Int] -> Field
    check field p [n] = flip conclude' p $ alter1 (`setField` n) field p
    check field _ _ = field

{- --- --- --- Backtracking --- --- --- -}
-- | following multiple paths
backtracking :: Field -> Field
backtracking field = may . filter solved . fmap solve $ backtrack field
  where
    may [] = field -- | may may fail
    may (f:fs) = f

-- | single backtracking step
backtrack :: Field -> [Field]
backtrack field = do
  let filterList = filter ((>= 2) . length . possibleNumbers) $ M.elems field
  guard (filterList /= [])
  let subject = minimumBy compareTile filterList
  f <- [flip setField x | x <- possibleNumbers subject]
  return $ alter1 f field (point subject)
  where
    compareTile :: SudokuTile -> SudokuTile -> Ordering -- | wtf ^^
    compareTile = (. (length . possibleNumbers)) . compare . length . possibleNumbers

{- --- --- --- Solving --- --- --- -}
solve :: Field -> Field
solve = solveList [exhausive conclusion, exhausive exclusion, backtracking]
  where
    solveList :: [Field -> Field] -> Field -> Field
    solveList [] field = field
    solveList (f:fs) field
      | solved field = field
      | otherwise = solveList fs $ f field

solved :: Field -> Bool
solved field = all (==[]) $ fmap ((\\)[1..9] . gatherNumbers field) elements
  where
    gatherNumbers :: Field -> Element -> [Int]
    gatherNumbers field = (=<<) (helper . maybe [] possibleNumbers . flip M.lookup field)

    helper :: [Int] -> [Int]
    helper [] = [0]
    helper [x] = [x]
    helper x = [0]

impossible :: Field -> Bool
impossible = any ((==0) . length . possibleNumbers) . M.elems

{- ----- ----- ----- ----- ----- Using what we've got ----- ----- ----- ----- ----- -}

file :: IO String
file = readFile "p96_sudoku.txt"

mkChunks :: Int -> [a] -> [[a]]
mkChunks n list
  | length list `mod` n == 0 = mkChunks' list
  | otherwise = []
  where
    mkChunks' [] = []
    mkChunks' x =
      let parts = splitAt n x
      in fst parts : mkChunks' (snd parts)

loadFields :: String -> [Field]
loadFields = fmap (mkField . mkTiles) . mkChunks 81 . mkIntList
  where
    mkIntList :: String -> [Int]
    mkIntList = (=<<) (fmap (\x->(read :: String -> Int) [x])) . fmap (filter isDigit) . filter ((/='G') . head) . lines

    mkTiles :: [Int] -> [SudokuTile]
    mkTiles = zipWith (\p n -> SudokuTile (nums n) p False) points
      where
        nums :: Int -> [Int]
        nums x
          | x == 0 = [1..9]
          | otherwise = [x]

    mkField :: [SudokuTile] -> Field
    mkField = M.fromAscList . fmap ((,) =<< point)

showField :: Field -> String
showField = unlines . mkChunks 9 . ((show . nums . possibleNumbers) =<<) . M.elems
  where
    nums :: [Int] -> Int
    nums [x] = x
    nums x = 0

fieldKey :: Field -> Integer
fieldKey = fromMaybe 0 . helper
  where
    helper :: Field -> Maybe Integer
    helper field = do
      f1 <- M.lookup (0,0) field
      f2 <- M.lookup (0,1) field
      f3 <- M.lookup (0,2) field
      let d1 = show . head $ possibleNumbers f1
      let d2 = show . head $ possibleNumbers f2
      let d3 = show . head $ possibleNumbers f3
      return $ read (d1 `mappend` d2 `mappend` d3)

main :: IO ()
main = do
  input <- file
  let fields = loadFields input
  let keys = fmap (fieldKey . solve) fields
  let number = sum keys
  print number
