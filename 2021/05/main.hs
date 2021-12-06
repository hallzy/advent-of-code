import Data.List

type Point = (Int, Int)
type Slope = (Int, Int)
type Points = [Point]
type Input = [Points]

getAllPointsOfHorizontalOrVerticalLine :: Point -> Point -> Points
getAllPointsOfHorizontalOrVerticalLine start end
    | startX == endX || startY == endY = getAllPointsOfLine start end
    | otherwise = []
    where
        (startX, startY) = start
        (endX, endY) = end

getAllPointsOfLine :: Point -> Point -> Points
getAllPointsOfLine start end
    | startX == endX && startY == endY = [end]
    | otherwise = start : getAllPointsOfLine newStart end
    where
        (startX, startY) = start
        (endX, endY) = end
        newStart = (startX + (xdiff `div` gcdXY), startY + (ydiff `div` gcdXY))
        xdiff = endX - startX
        ydiff = endY - startY
        gcdXY = gcd xdiff ydiff

split :: Char -> String -> (Int, Int)
split delim str = tuplify $ lines $ map aux str
    where
        aux item
            | item == delim = '\n'
            | otherwise = item
        tuplify :: [String] -> (Int, Int)
        tuplify lst = (read $ head lst, read $ last lst)

getInput :: (Point -> Point -> Points) -> String -> Input
getInput getPointsFunc = map ((\x -> getPointsFunc (head x) (last x)) . map (split ',') . filter (/= "->") . words) . lines

solve :: Input -> String
solve = show . length . filter (> 1) . map length . group . sort . foldl1 (++)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let input1 = getInput getAllPointsOfHorizontalOrVerticalLine contents
    let input2 = getInput getAllPointsOfLine contents
    putStrLn $ "Part 1: " ++ (solve input1)
    putStrLn $ "Part 2: " ++ (solve input2)
