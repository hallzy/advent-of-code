solve1 :: [Int] -> Int
solve1 input = countIncrease 0 input
    where
        countIncrease count (x:y:ns)
            | y > x = countIncrease (count + 1) (y : ns)
            | otherwise = countIncrease count (y : ns)
        countIncrease count _ = count

solve2 :: [Int] -> Int
solve2 input = countIncrease 0 input
    where
        countIncrease count (w:x:y:z:ns)
            | (w + x + y) < (x + y + z) = countIncrease (count + 1) (x : y : z : ns)
            | otherwise = countIncrease count (x : y : z : ns)
        countIncrease count _ = count


betterSolution1 :: [Int] -> Int
betterSolution1 xs = count True $ zipWith (<) xs (tail xs)
    where
        count needle haystack = length $ filter (== needle) haystack

betterSolution2 :: [Int] -> Int
betterSolution2 xs@(_:ys@(_:zs)) = betterSolution1 $ zipWith3 (\x y z -> sum [x,y,z]) xs ys zs


main :: IO ()
main = do
    contents <- readFile "input.txt"
    let input = map read $ lines contents
    putStrLn $ "Part 1: " ++ (show $ solve1 input)
    putStrLn $ "Part 2: " ++ (show $ solve2 input)
    putStrLn ""
    putStrLn $ "Better 1: " ++ (show $ betterSolution1 input)
    putStrLn $ "Better 2: " ++ (show $ betterSolution2 input)
