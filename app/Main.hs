module Main where

import           Control.Applicative ((<|>))
import           Data.Char           (digitToInt, intToDigit, isDigit)
import           Data.Function       (on)
import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as IntMap
import           Data.List           (findIndices, foldl', groupBy, minimumBy,
                                      sort, transpose)
import           Data.Tuple          (swap)

puz = readGrid ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."
puz2 = readGrid ".......1.4.........2...........5.6.4..8...3....1.9....3..4..2...5.1........8.7..."
puz3 = readGrid ".......12....35......6...7.7.....3.....4..8..1...........12.....8.....4..5....6.."
puz4 = readGrid ".6....3..4..7............8......8.125..6............5..82...7.....5..6......1...."

-- type Cell = [Int]
data Cell = Fixed Int | Possible [Int] deriving (Eq, Show)
type Row = [Cell]
type Grid = [Row]

isFixedCell :: Cell -> Bool
isFixedCell (Fixed _) = True
isFixedCell _         = False

isValidCell :: Cell -> Bool
isValidCell (Fixed _)     = True
isValidCell (Possible []) = False
isValidCell _             = True

chunkOf :: Int -> [a] -> [[a]]
chunkOf _ [] = []
chunkOf n xs = take n xs : chunkOf n (drop n xs)

readGrid :: String -> Grid
readGrid = chunkOf 9 . map readCell
  where
    readCell :: Char -> Cell
    readCell '.'            = Possible [1..9]
    readCell c  | isDigit c = Fixed $ digitToInt c
                | otherwise = error "Invalid puzzle source.  Contains non-digit character."

showGrid :: Grid -> String
showGrid [r1,r2,r3,r4,r5,r6,r7,r8,r9]
    = "+-------+-------+-------+\n"
        <> showRow r1 <> "\n"
        <> showRow r2 <> "\n"
        <> showRow r3 <> "\n"
        <> "+-------+-------+-------+\n"
        <> showRow r4 <> "\n"
        <> showRow r5 <> "\n"
        <> showRow r6 <> "\n"
        <> "+-------+-------+-------+\n"
        <> showRow r7 <> "\n"
        <> showRow r8 <> "\n"
        <> showRow r9 <> "\n"
        <> "+-------+-------+-------+\n"
  where
    showCell :: Cell -> String
    showCell (Fixed n)    = show n
    showCell (Possible _) = "."

    showRow [c1,c2,c3,c4,c5,c6,c7,c8,c9]
        =       "| " <> showCell c1 <> " " <> showCell c2 <> " " <> showCell c3
            <> " | " <> showCell c4 <> " " <> showCell c5 <> " " <> showCell c6
            <> " | " <> showCell c7 <> " " <> showCell c8 <> " " <> showCell c9 <> " |"

showGridWithPossibilities :: Grid -> String
showGridWithPossibilities = unlines . map showRow
  where
    showCell :: Cell -> String
    showCell (Fixed n)      = "     " <> show n <> "     "
    showCell (Possible ps)  = "[" <> map (\n -> if n `elem` ps then intToDigit n else ' ') [1..9] <> "]"

    showRow :: Row -> String
    showRow = unwords . map showCell

reverseTranspose = transpose

subGrid :: [[a]] -> [[a]]
subGrid = map concat . concatMap (chunkOf 3) . transpose . map (chunkOf 3)

reverseSubGrid = subGrid . subGrid

pruneFixedInRow :: Row -> Row
pruneFixedInRow row = map prune row
  where
    catFixed = [n | Fixed n <- row]

    prune (Fixed n)     = Fixed n
    prune (Possible ps) = Possible $ filter (`notElem` catFixed) ps

possiblitiesToCell :: [Int] -> Cell
possiblitiesToCell [n] = Fixed n
possiblitiesToCell ns  = Possible ns

type OccurrenceMap = IntMap [Int]

occurrences :: Row -> OccurrenceMap
occurrences = foldr (\(index, possibilities) occMap -> occurrencesAt index occMap possibilities) IntMap.empty . indexAndTrimFixed
 where
    indexAndTrimFixed :: Row -> [(Int, [Int])]
    indexAndTrimFixed = (\xs -> [(index, ds) | (index, Possible ds) <- xs ]) . zip [0..8]

    prependOccurrenceIndexOf :: Int -> Int -> OccurrenceMap -> OccurrenceMap
    prependOccurrenceIndexOf digit index = IntMap.insertWith (\(new:_) old -> new:old) digit [index]

    occurrencesAt :: Int -> OccurrenceMap -> [Int] -> OccurrenceMap
    occurrencesAt index = foldr (\digit occMap -> prependOccurrenceIndexOf digit index occMap)

prunables :: Row -> [([Int], Cell)]
prunables = convertToCell . filterPrunable . mergeInsideGroup . groupeOccurrences . map swap . IntMap.toList . occurrences
  where
    groupeOccurrences :: (Eq a, Ord a, Ord b) => [(a, b)] -> [[(a, b)]]
    groupeOccurrences = groupBy (\(xs, _) (ys, _) -> xs == ys) . sort

    mergeInsideGroup :: [[([a], b)]] -> [([a], [b])]
    mergeInsideGroup = map (\g -> ((fst . head) g, map snd g))

    filterPrunable :: [([a], [b])] -> [([a], [b])]
    filterPrunable = filter (\(x, y) -> length x == length y)

    convertToCell = map (\(indices, possibilities) -> (indices, possiblitiesToCell possibilities))

pruneRow :: Row -> Row
pruneRow = prunePrunable . pruneFixedInRow
  where
    prunePrunable row = foldl' (\acc (indices, replacement) -> replaceNths replacement indices acc) row $ prunables row

prune :: Grid -> Grid
prune = pruneSubGrids . pruneColumns . pruneRows
  where
    pruneRows       = map pruneRow
    pruneColumns    = reverseTranspose . pruneRows . transpose
    pruneSubGrids   = reverseSubGrid . pruneRows . subGrid

replaceNths
    :: a        -- ^ Replacement
    -> [Int]    -- ^ List of indices where the replacement will be put
    -> [a]      -- ^ Target list to replace Nths elements
    -> [a]
replaceNths replacement = go 0
  where
    go _     _          []                      = []
    go _     []         xs                      = xs
    go currI ind@(i:is) (x:xs)  | currI == i    = replacement : go (currI + 1) is xs
                                | otherwise     = x : go (currI + 1) ind xs

settle :: Grid -> Grid
settle old = let new = prune old in if new == old then new else settle new

isFixed :: Grid -> Bool
isFixed = all isFixedCell . concat

isInvalid :: Grid -> Bool
isInvalid = any (not . isValidCell) . concat

isSolved :: Grid -> Bool
isSolved grid = isFixed grid && not (hasDup grid)
  where
    hasDupInRow :: Row -> Bool
    hasDupInRow [] = False
    hasDupInRow (x:xs)  | x `elem` xs = True
                        | otherwise = hasDupInRow xs

    hasDupInRows :: Grid -> Bool
    hasDupInRows = any hasDupInRow
    hasDupInColumns = hasDupInRows . transpose
    hasDupInSubGrids = hasDupInRows . subGrid

    hasDup grid = hasDupInSubGrids grid || hasDupInColumns grid || hasDupInRows grid

findLeastPossibleCellIndex :: Grid -> Int
findLeastPossibleCellIndex = fst . minimumBy (compare `on` snd) . numPossible . zip [0..] . concat
  where
    numPossible xs = [(index, length ps) | (index, Possible ps) <- xs]

findLeastPossibleCell :: Grid -> Int -> Cell
findLeastPossibleCell grid index = concat grid !! index

replaceNthCell :: Int -> Cell -> Grid -> Grid
replaceNthCell index replacement grid = chunkOf 9 $ go 0 $ concat grid
  where
    go _     []                         = []
    go currI (x:xs) | currI == index    = replacement : xs
                    | otherwise         = x : go (currI + 1) xs


branch :: Grid -> (Grid, Grid)
branch grid = (firstGrid, secondGrid)
  where
    index               = findLeastPossibleCellIndex grid
    (Possible (x:xs))   = findLeastPossibleCell grid index
    firstGrid           = replaceNthCell index (Fixed x) grid
    secondGrid          = replaceNthCell index (Possible xs) grid

solve :: Grid -> Maybe Grid
solve grid  | isSolved settled  = Just settled  -- Solved
            | isFixed settled   = Nothing       -- Fixed but not solved.  Unresolvable!
            | isInvalid settled = Nothing       -- Containing impossible cell!
            | otherwise         = solve first <|> solve next
  where
    settled         = settle grid
    (first, next)   = branch settled

main :: IO ()
main = do
    srcs <- getContents
    mapM_ (putStrLn . showResult . (solve . readGrid)) $ lines srcs
  where
    showResult Nothing     = "No solution!"
    -- showResult (Just _)  = "."
    showResult (Just grid) = showGrid grid
