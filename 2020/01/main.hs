sumTarget :: Int
sumTarget = 2020

-- Change to 2 for Part 1, change to 3 for part 2
howManyEntries :: Int
howManyEntries = 3

check :: [Int] -> Bool
check list = aux list
    where
        aux (x:y:ns)
            | x < y = if (length ns == 0) then True else aux (y : ns)
            | otherwise = False

combinations :: [Int] -> [[Int]]
combinations input = [ x | x <- sequence (replicate howManyEntries input), check x]

matchingTuple :: [Int] -> [Int]
matchingTuple input = aux $ combinations input
    where
        aux (n:ns)
            | (sum n) == sumTarget = n
            | otherwise = aux ns

solve :: [Int] -> Int
solve input = product $ matchingTuple input

main :: IO ()
main = do
    contents <- readFile "input.txt"
    putStrLn $ show $ solve $ map read $ lines contents
