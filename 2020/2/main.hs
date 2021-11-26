data Input = Input { min :: Int, max :: Int, letter :: Char, password :: String } deriving Show

str2Input :: String -> Input
str2Input str = Input min max letter password
    where
        arr      = words str
        min      = read $ takeWhile (/= '-') $ head arr
        max      = read $ tail $ dropWhile (/= '-') $ head arr
        letter   = head $ head $ tail arr
        password = last arr

-- This is for Part 1
-- validateInput :: Input -> Bool
-- validateInput (Input min max letter password) = count >= min && count <= max
--     where
--         count = length $ filter (== letter) password

-- This is for Part 2
validateInput :: Input -> Bool
validateInput (Input min max letter password) = ((password !! (min - 1)) == letter) /= ((password !! (max - 1)) == letter)

solve :: [String] -> String
solve ns = show $ validCounts 0 ns
    where
        validCounts count [] = count
        validCounts count (n:ns)
          | validateInput (str2Input n) = validCounts (count + 1) ns
          | otherwise = validCounts count ns


main :: IO ()
main = do
    contents <- readFile "input.txt"
    putStrLn $ solve $ lines contents
