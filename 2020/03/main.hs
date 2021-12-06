type Slope = (Int, Int)
type Board = [String]

slopes :: [Slope]
slopes = [
        (1, 1),
        (3, 1),  -- This is the only input for part 1
        (5, 1),
        (7, 1),
        (1, 2)
    ]

str2Board :: String -> Board
str2Board str = lines str

solve :: String -> String
solve input = show $ count 0 slopes (0, 0) board
    where
        board = str2Board input
        width = length $ board !! 0
        height = length board
        count _ [] _ _ = 1
        count trees (slope:ss) (x,y) board
            | y >= height = trees * (count 0 ss (0, 0) board)
            | x >= width = count trees (slope:ss) (x - width, y) board
            | isTree = count (trees + 1) (slope:ss) (newX, newY) board
            | otherwise = count trees (slope:ss) (newX, newY) board
            where
                isTree = board !! y !! x == '#'
                newX = x + (fst slope)
                newY = y + (snd slope)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    putStrLn $ solve contents
