import Data.List

split :: Char -> String -> [Int]
split delim str = map read $ lines $ map aux str
    where
        aux item
            | item == delim = '\n'
            | otherwise = item

calculateFuel1 :: Int -> [(Int, Int)] -> Int
calculateFuel1 idx groupedList = foldl (\acc x -> abs(idx - (fst x)) * (snd x) + acc) 0 $ groupedList

calculateFuel2 :: Int -> [(Int, Int)] -> Int
calculateFuel2 idx groupedList = foldl (\acc x -> (sum [1..abs(idx - (fst x))]) * (snd x) + acc) 0 $ groupedList

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
    putStrLn $ "Part 1: " ++ (solve 2 input)
