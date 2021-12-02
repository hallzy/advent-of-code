-- Part 1
solve1 :: [(String, Int)] -> Int
solve1 input = aux (0, 0) input
    where
        aux (startX, startY) ((direction, amount):xs)
          | direction == "forward" = aux (startX + amount, startY)          xs
          | direction == "down"    = aux (startX,          startY + amount) xs
          | direction == "up"      = aux (startX,          startY - amount) xs
          | otherwise              = aux (startX,          startY)          xs
        aux (startX, startY) _ = startX * startY

-- Part 2
solve2 :: [(String, Int)] -> Int
solve2 input = aux (0, 0, 0) input
    where
        aux (startX, startY, aim) ((direction, amount):xs)
          | direction == "forward" = aux (startX + amount, startY + (aim * amount), aim)          xs
          | direction == "down"    = aux (startX,          startY,                  aim + amount) xs
          | direction == "up"      = aux (startX,          startY,                  aim - amount) xs
          | otherwise              = aux (startX,          startY,                  aim)          xs
        aux (startX, startY, _) _  = startX * startY

betterSolution1 :: [(String, Int)] -> Int
betterSolution1 xs = x * y
    where
        (x, y) = foldl move (0, 0) xs
        move (x, y) ("forward", magnitude) = (x + magnitude, y)
        move (x, y) ("down", magnitude)    = (x,             y + magnitude)
        move (x, y) ("up", magnitude)      = (x,             y - magnitude)

betterSolution2 :: [(String, Int)] -> Int
betterSolution2 xs = x * y
    where
        (x, y, _) = foldl move (0, 0, 0) xs
        move (x, y, a) ("forward", magnitude) = (x + magnitude, y + (a * magnitude), a)
        move (x, y, a) ("down", magnitude)    = (x,             y                  , a + magnitude)
        move (x, y, a) ("up", magnitude)      = (x,             y                  , a - magnitude)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let input = map (\x -> (head $ words x, read $ last $ words x)) $ lines contents
    putStrLn $ "Part 1: " ++ (show $ solve1 input)
    putStrLn $ "Part 2: " ++ (show $ solve2 input)
    putStrLn $ ""
    putStrLn $ "Better 1: " ++ (show $ betterSolution1 input)
    putStrLn $ "Better 2: " ++ (show $ betterSolution2 input)
