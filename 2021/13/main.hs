import Data.List
import Data.Maybe
import Data.Char

split :: Char -> String -> (Int, Int)
split delim str = (yPos, xPos)
   where
      xPos = read $ head arr
      yPos = read $ last arr
      arr = lines $ map aux str
      aux item
         | item == delim = '\n'
         | otherwise = item

splitFold :: Char -> String -> (String, Int)
splitFold delim str = (axis, pos)
   where
      axis = head arr
      pos = read $ last arr
      arr = lines $ map aux str
      aux item
         | item == delim = '\n'
         | otherwise = item

inputToBoard :: [(Int, Int)] -> [[Bool]]
inputToBoard input = aux 0
   where
      numRows = (maximum $ map (fst) input) + 1
      numCols = (maximum $ map (snd) input) + 1
      aux :: Int -> [[Bool]]
      aux curRow
        | curRow >= numRows             = []
        | otherwise                     = getRow 0 : aux (curRow + 1)
         where
            getRow :: Int -> [Bool]
            getRow curCol
              | curCol >= numCols             = []
              | (curRow, curCol) `elem` input = [ True ]  ++ getRow (curCol + 1)
              | otherwise                     = [ False ] ++ getRow (curCol + 1)

doFolds :: [(String, Int)] -> [[Bool]] -> [[Bool]]
doFolds [] board = board
doFolds ((axis, value):folds) board
  | axis == "y" = doFolds folds (foldY value board)
  | axis == "x" = doFolds folds (foldX value board)
  | otherwise = error $ "Unknown axis: " ++ axis
   where
      fold twoHalves = zipWith (zipWith (\c1 c2 -> any (== True) [c1, c2])) (head twoHalves) (last twoHalves)
      foldY y board = fold [take y board, reverse $ drop (y + 1) board]
      foldX x board = fold [map (take x) board, map (reverse . drop (x + 1)) board]

solve1 :: [[Bool]] -> [(String, Int)] -> String
solve1 board folds = show $ sum $ map (sum . map (fromEnum)) $ doFolds folds board

solve2 :: [[Bool]] -> [(String, Int)] -> String
solve2 board folds = unlines $ map (map (\x -> if x then '#' else ' ')) $ doFolds folds board

main :: IO ()
main = do
   contents <- readFile "input.txt"
   let input = lines contents
   let splitIdx = fromMaybe 0 $ elemIndex "" input
   let coords = sort $ map (split ',') $ take splitIdx input
   let board = inputToBoard coords
   let foldInstructions = map (splitFold '=' . drop 11) $ drop (splitIdx + 1) input
   putStrLn $ "Part 1: " ++ (solve1 board (take 1 foldInstructions))
   putStrLn $ "Part 2:\n" ++ (solve2 board foldInstructions)
