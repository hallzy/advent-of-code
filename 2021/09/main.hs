import Data.Maybe
import Data.List
import Debug.Trace

getFlatAdjacency :: [[Int]] -> [(Int, (Int, Int), [Int])]
getFlatAdjacency = foldl (++) [] . getAdjacency

getAdjacency :: [[Int]] -> [[(Int, (Int, Int), [Int])]]
getAdjacency list = aux 0
   where
      rows = length list
      cols = length (list !! 0)

      outOfBounds :: Int -> Int -> Bool
      outOfBounds rowIdx colIdx = rowIdx < 0 || rowIdx >= rows || colIdx < 0 || colIdx >= cols

      maybeAccess :: Int -> Int -> Maybe Int
      maybeAccess rowIdx colIdx
         | outOfBounds rowIdx colIdx = Nothing
         | otherwise                 = Just (list !! rowIdx !! colIdx)

      aux :: Int -> [[(Int, (Int, Int), [Int])]]
      aux rowIdx
         | rows == rowIdx = []
         | otherwise      = auxRow 0 : aux (rowIdx + 1)

         where
            auxRow :: Int -> [(Int, (Int, Int), [Int])]
            auxRow colIdx
               | cols == colIdx = []
               | otherwise      = (value, (rowIdx, colIdx), map (fromMaybe 9) [above, below, left, right]) : auxRow (colIdx + 1)
               where
                  value = list !! rowIdx !! colIdx
                  above = maybeAccess (rowIdx - 1) colIdx
                  below = maybeAccess (rowIdx + 1) colIdx
                  left  = maybeAccess rowIdx (colIdx - 1)
                  right = maybeAccess rowIdx (colIdx + 1)

char2str :: Char -> String
char2str = (: [])

lowPoints :: [[Int]] -> [(Int, (Int, Int), [Int])]
lowPoints = filter (\(val, _, adj) -> val < (minimum adj)) . getFlatAdjacency

basinSizes :: [[Int]] -> [[(Int, Int)]]
basinSizes list = traverse (lowPoints list)
   where
      adjacency :: [[(Int, (Int, Int), [Int])]]
      adjacency = getAdjacency list

      traverse :: [(Int, (Int, Int), [Int])] -> [[(Int, Int)]]
      traverse [] = []
      traverse ((toTraverse@(_, (rowIdx, colIdx), _)):startPoints) = (traverseBasin toTraverse []) : traverse startPoints
         where
            traverseBasin :: (Int, (Int, Int), [Int]) -> [(Int, Int)] -> [(Int, Int)]
            traverseBasin toTraverse traversed = traversedAll
               where
                  (value, (rowIdx, colIdx), (above:below:left:right:[])) = toTraverse
                  traversedAll
                    | alreadyTraversed = map head $ group $ sort $traversed
                    | otherwise = map head $ group $ sort $ traverseRight
                     where
                        alreadyTraversed = (rowIdx, colIdx) `elem` traversed
                        newTraversed
                          | alreadyTraversed = traversed
                          | otherwise = (rowIdx, colIdx) : traversed
                        traverseUp
                          | value == 9 || rowIdx == 0 = traversed
                          | otherwise = traverseBasin (adjacency !! (rowIdx - 1) !! colIdx) newTraversed
                        traverseDown
                          | value == 9 || rowIdx == (length list) - 1 = traverseUp ++ traversed
                          | otherwise = traverseBasin (adjacency !! (rowIdx + 1) !! colIdx) (traverseUp ++ newTraversed)
                        traverseLeft
                          | value == 9 || colIdx == 0 = traverseDown ++ traversed
                          | otherwise = traverseBasin (adjacency !! rowIdx !! (colIdx - 1)) (traverseDown ++ newTraversed)
                        traverseRight
                          | value == 9 || colIdx == (length (list !! 0)) - 1 = traverseLeft ++ traversed
                          | otherwise = traverseBasin (adjacency !! rowIdx !! (colIdx + 1)) (traverseLeft ++ newTraversed)


solve1 :: [[Int]] -> String
solve1 = show . sum . map (\(val,_,_) -> val + 1) . lowPoints

solve2 :: [[Int]] -> String
solve2 input = show . product . take 3 . reverse . sort . map length $ basinSizes input

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let input = map (map (read . char2str)) $ lines contents
    putStrLn $ "Part 1: " ++ (solve1 input)
    putStrLn $ "Part 2: " ++ (solve2 input)
