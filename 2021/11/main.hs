import Data.Maybe
import Data.List

type Location = (Int, Int)

listOfCoords :: [[Int]] -> [Location]
listOfCoords octopuses = [(row, col) | row <- [0..(numRows - 1)], col <- [0..(numCols - 1)]]
   where
      numRows = length octopuses
      numCols = length (octopuses !! 0)

replaceEl :: Int -> [a] -> a -> [a]
replaceEl idx list newValue = take idx list ++ [newValue] ++ drop (idx + 1) list

replaceEl2 :: Location -> [[a]] -> a -> [[a]]
replaceEl2 (row, col) list newValue = replaceEl row list $ replaceEl col (list !! row) newValue

getAdjacent :: [[Int]] -> Location -> [Location]
getAdjacent octopuses (row, col) = filter (\adj -> adj `elem` (listOfCoords octopuses)) adjacentCoords
   where
      adjacentCoords = [
         (row - 1, col - 1), (row - 1, col), (row - 1, col + 1),
         (row,     col - 1), (row    , col), (row    , col + 1),
         (row + 1, col - 1), (row + 1, col), (row + 1, col + 1)
         ]

getMultiAdjacent :: [[Int]] -> [Location] -> [Location]
getMultiAdjacent octopuses [] = []
getMultiAdjacent octopuses (loc:locs) = (getAdjacent octopuses loc) ++ (getMultiAdjacent octopuses locs)

char2str :: Char -> String
char2str = (: [])

flash :: [[Int]] -> [Location] -> [[Int]]
flash octopuses [] = octopuses
flash octopuses (loc:locs) = flash (replaceEl2 loc octopuses 0) locs

locationsToFlash :: [[Int]] -> [Location]
locationsToFlash octopuses = aux (listOfCoords octopuses) []
   where
      aux [] locations = locations
      aux (coord:coords) locations
         | curVal > 9 = aux coords $ coord : locations
         | otherwise  = aux coords locations
         where
            curRow = fst coord
            curCol = snd coord
            curVal = octopuses !! curRow !! curCol

increaseEls :: [Location] -> [[Int]] -> [[Int]]
increaseEls [] octopuses = octopuses
increaseEls ((row, col):locs) octopuses
   | val == 0    = increaseEls locs octopuses
   | otherwise   = increaseEls locs increased
   where
      val = octopuses !! row !! col
      increased = replaceEl2 (row, col) octopuses (val + 1)

doStep :: Int -> [[Int]] -> (Int, [[Int]])
doStep flashCount octopuses
   | toFlash == [] = (flashCount, octopuses)
   | otherwise     = doStep (flashCount + newFlashCount) adjToFlashIncreased
   where
      toFlash = locationsToFlash octopuses
      newFlashCount = length toFlash
      flashed = flash octopuses toFlash
      adjToFlashIncreased = increaseEls (getMultiAdjacent flashed toFlash) flashed

step :: Int -> Int -> [[Int]] -> (Int, [[Int]])
step 0 flashCount octopuses = (flashCount, octopuses)
step stepsLeft flashCount octopuses = step (stepsLeft - 1) (flashCount + (fst stepResult)) $ snd stepResult
   where
      increased = map (map (+ 1)) octopuses
      stepResult = doStep 0 increased

allZero :: [[Int]] -> Bool
allZero = all (== True) . map (all (== 0))

solve1 :: [[Int]] -> String
solve1 = show . fst . step 100 0

solve2 :: [[Int]] -> String
solve2 = show . aux 0
   where
      aux stepCount octopuses
         | allZero (snd $ step stepCount 0 octopuses) = stepCount
         | stepCount > 500 = error $ "Step Count is " ++ (show stepCount)
         | otherwise = aux (stepCount + 1) octopuses

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let input =  map (map (read . char2str)) $ lines contents
    putStrLn $ "Part 1: " ++ (solve1 input)
    putStrLn $ "Part 2: " ++ (solve2 input)
