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

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let input = map (\x -> (head $ words x, read $ last $ words x)) $ lines contents
    putStrLn $ "Part 1: " ++ (show $ solve1 input)
    putStrLn $ "Part 2: " ++ (show $ solve2 input)
