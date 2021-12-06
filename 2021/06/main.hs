import Data.List

initialLookup :: [Int]
initialLookup = take 9 $ repeat 0

replaceEl :: Int -> [a] -> a -> [a]
replaceEl idx list newValue = take idx list ++ [newValue] ++ drop (idx + 1) list

runGeneration :: [Int] -> [Int]
runGeneration list = aux 0 list
    where
        aux idx list
            | idx == length list = []
            | idx == 6           = (list !! 7) + (list !! 0) : recurse
            | idx == 8           = (list !! 0)               : recurse
            | otherwise          = (list !! (idx + 1))       : recurse
            where
                recurse = aux (idx + 1) list

runGenerations :: Int -> [Int] -> [Int]
runGenerations 0 list = list
runGenerations count list = runGeneration $ runGenerations (count - 1) list

split :: Char -> String -> [Int]
split delim str = map read $ lines $ map aux str
    where
        aux item
            | item == delim = '\n'
            | otherwise = item

solve :: Int -> [Int] -> String
solve count list = show $ sum $ runGenerations count list

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let input = foldl (\acc x -> replaceEl x acc ((acc !! x) + 1)) initialLookup $ split ',' contents
    putStrLn $ "Part 1: " ++ (solve 80 input)
    putStrLn $ "Part 2: " ++ (solve 256 input)
