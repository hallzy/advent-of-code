import Data.List
import Data.Char (isSpace)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

split :: Char -> String -> [String]
split delim str = lines $ map aux str
    where
        aux item
            | item == delim = '\n'
            | otherwise = item

isUniqueSize :: String -> Bool
isUniqueSize str = len == 2 || len == 4 || len == 3 || len == 7
    where
        len = length str

uniqueFind :: Int -> [String] -> String
uniqueFind length (x:xs)
    | length x == length = x
    | otherwise     = uniqueFind xs

permuteFind :: Int -> String -> [String] -> String
permuteFind length toPermute (x:xs)
    | length x == length && x `elem` permutations toPermute = x
    | otherwise     = permuteFind length toPermute xs

permuteNotFind :: Int -> String -> [String] -> String
permuteNotFind length toPermute (x:xs)
    | length x == length && not $ x `elem` permutations toPermute = x
    | otherwise     = permuteNotFind length toPermute xs

permuteFindAllOf :: Int -> [String] -> [String] -> String
permuteFindAllOf length toPermute (x:xs)
    | length x == length && not $ x `elem` allPermutations toPermute = x
    | otherwise     = permuteFindAllOf length toPermute xs
    where
        allPermutations [] = []
        allPermutations (x:xs) = permutations x ++ allPermutations xs

find' :: Int -> [String] -> String
find' numToFind xs
    | numToFind == 0 = uniqueFind 6 xs  -- Only 6 character long value left after several others have already been found
    | numToFind == 1 = uniqueFind 2 xs
    | numToFind == 2 = undefined
    | numToFind == 3 = permuteFind 5 (find' 1 xs) xs
    | numToFind == 4 = uniqueFind 4 xs
    | numToFind == 5 = undefined
    | numToFind == 6 = permuteNotFind 6 (find' 1 xs) xs
    | numToFind == 7 = uniqueFind 3 xs
    | numToFind == 8 = uniqueFind 7 xs
    | numToFind == 9 = permuteFind 6 (find' 4 xs) xs

inputToTuples :: [String] -> [(Int, String)]
inputToTuples [] = []
inputToTuples (x:xs) = (-1, x) : inputToTuples xs

deduce :: [(Int, String)] -> [(Int, String)]

solve1 :: [[[String]]] -> Int
solve1 [] = 0
solve1 (x:xs) = (aux x) + (solve1 xs)
    where
        aux (_:output:[]) = (length $ filter isUniqueSize output)
        aux _ = error "Invalid input"

solve2 :: [[[String]]] -> Int
solve2 [] = 0
solve2 (x:xs) = (aux x) + (solve2 xs)
    where
        aux (_:output:[]) = (length $ filter isUniqueSize output)
        aux _ = error "Invalid input"

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let input = map (map (words . trim) . split '|') $ lines contents
    putStrLn $ "Part 1: " ++ (show $ solve1 input)
    putStrLn $ "Part 2: " ++ (show $ solve2 input)
