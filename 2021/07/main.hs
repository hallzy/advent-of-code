import Data.List

split :: Char -> String -> [Int]
split delim str = map read $ lines $ map aux str
    where
        aux item
            | item == delim = '\n'
            | otherwise = item

-- Triangle number of n is n + (n - 1) + (n - 2) + ... + 1
-- Yes, I could do a sum of [1..n], but that is significantly slower for this
-- use case
triangleNumber :: Int -> Int
triangleNumber n = n*(n + 1) `div` 2

calculateFuel1 :: Int -> [(Int, Int)] -> Int
calculateFuel1 idx groupedList = foldl' (\acc (pos, count) -> acc + count * abs(idx - pos)) 0 groupedList

calculateFuel2 :: Int -> [(Int, Int)] -> Int
calculateFuel2 idx groupedList = foldl' (\acc (pos, count) -> acc + count * (triangleNumber $ abs(idx - pos))) 0 groupedList

solve :: Int -> [Int] -> String
solve part list = show $ minimum $ aux min
    where
        grouped = map (\x -> (head x, length x)) $ group $ sort list

        max = maximum list
        min = minimum list

        aux :: Int -> [Int]
        aux idx
          | idx > max = []
          | otherwise = calculateFuel idx : aux (idx + 1)

        calculateFuel idx
            | part == 1 = calculateFuel1 idx grouped
            | part == 2 = calculateFuel2 idx grouped

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let input = split ',' contents
    putStrLn $ "Part 1: " ++ (solve 1 input)
    putStrLn $ "Part 2: " ++ (solve 2 input)
