import Data.List
import Data.Char
import qualified Data.Map as Map

split :: Char -> String -> [String]
split delim str = lines $ map aux str
    where
        aux item
            | item == delim = '\n'
            | otherwise = item

getPathsFrom1 :: String -> Map.Map String Int -> Map.Map String [String] -> [String]
getPathsFrom1 point visited allPaths = filter (\x -> Map.notMember x visited) $ allPaths Map.! point

getPathsFrom2 :: String -> Map.Map String Int -> Map.Map String [String] -> [String]
getPathsFrom2 point visited allPaths
   | visitedHas2SmallCaves = getPathsFrom1 point visited allPaths
   | otherwise = filter (not . (\x -> x `elem` ["start"])) $ allPaths Map.! point
   where
      visitedCounts = map (snd) $ Map.toList $ Map.filterWithKey (\key _ -> key /= "start") visited
      visitedHas2SmallCaves = 1 < if visitedCounts == [] then 0 else (maximum $ visitedCounts)

newInputList :: [[String]] -> [String] -> [[String]]
newInputList input@(current:rest) [] = rest
newInputList input@(current:rest) (path:[]) = ((current ++ [path]) : rest)
newInputList input@(current:rest) (path:paths) = newInputList (input ++ [current ++ [path]]) paths

getPaths :: Int -> [[String]] -> Map.Map String [String] -> Int
getPaths _ [] _ = 0
getPaths version input@(currentPath:rest) allPaths
   | currentPoint == "end" = 1 + (getPaths version rest allPaths)
   | otherwise = getPaths version (newInputList input paths) allPaths
   where
      currentPoint = last currentPath
      visited = foldl' (\acc node -> Map.insertWith (+) node 1 acc) Map.empty $ filter isLowerStr currentPath
      getPathsFunc = if version == 1 then getPathsFrom1 else getPathsFrom2
      paths = getPathsFunc currentPoint visited allPaths

isLowerStr :: String -> Bool
isLowerStr = all isLower

solve1 :: Map.Map String [String] -> String
solve1 = show . getPaths 1 [["start"]]

solve2 :: Map.Map String [String] -> String
solve2 = show . getPaths 2 [["start"]]

main :: IO ()
main = do
   contents <- readFile "input.txt"
   let input = foldl' (\acc (node1:node2:[]) -> Map.insertWith (++) node2 [node1] $ Map.insertWith (++) node1 [node2] acc) Map.empty $ map (split '-') $ lines contents
   putStrLn $ "Part 1: " ++ (solve1 input)
   putStrLn $ "Part 2: " ++ (solve2 input)
