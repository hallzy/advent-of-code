import Data.List
import Data.Char

split :: Char -> String -> [String]
split delim str = lines $ map aux str
    where
        aux item
            | item == delim = '\n'
            | otherwise = item

list2tuple :: [String] -> (String, String)
list2tuple (first:second:[])= (first, second)

getPathsFrom1 :: String -> [String] -> [(String, String)] -> [String]
getPathsFrom1 point visited allPaths = filter (not . (\x -> x `elem` visited)) $ map (\(a,b) -> if a == point then b else a) $ filter (\(a, b) -> any (== point) [a,b]) allPaths

getPathsFrom2 :: String -> [String] -> [(String, String)] -> [String]
getPathsFrom2 point visited allPaths = filter filterOutVisited $ map (\(a,b) -> if a == point then b else a) $ filter (\(a, b) -> any (== point) [a,b]) allPaths
   where
      visitedCounts = map length $ group $ sort $ filter (/= "start") visited
      visitedHas2SmallCaves = 1 < if visitedCounts == [] then 0 else (maximum $ visitedCounts)
      filterOutVisited = if visitedHas2SmallCaves then (not . (\x -> x `elem` visited)) else (not . (\x -> x `elem` ["start"]))

newInputList :: [[String]] -> [String] -> [[String]]
newInputList input@(current:rest) [] = rest
newInputList input@(current:rest) (path:[]) = ([newEl] ++ rest)
   where
      newEl = current ++ [path]
newInputList input@(current:rest) (path:paths) = newInputList (input ++ [newEl]) paths
   where
      newEl = current ++ [path]

getPaths :: Int -> [[String]] -> [(String, String)] -> [[String]]
getPaths _ [] _ = []
getPaths version input@(currentPath:rest) allPaths
   | currentPoint == "end" = currentPath : getPaths version rest allPaths
   | otherwise = getPaths version (newInputList input paths) allPaths
   where
      currentPoint = last currentPath
      visited = filter isLowerStr currentPath
      getPathsFunc = if version == 1 then getPathsFrom1 else getPathsFrom2
      paths = getPathsFunc currentPoint visited allPaths

isLowerStr :: String -> Bool
isLowerStr = all isLower

solve1 :: [(String, String)] -> String
solve1 = show . length . getPaths 1 [["start"]]

solve2 :: [(String, String)] -> String
solve2 = show . length . getPaths 2 [["start"]]

main :: IO ()
main = do
   contents <- readFile "input.txt"
   let input = map (list2tuple . split '-') $ lines contents
   putStrLn $ "Part 1: " ++ (solve1 input)
   putStrLn $ "Part 2: " ++ (solve2 input)
