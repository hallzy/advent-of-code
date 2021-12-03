char2int :: Char -> Int
char2int x = read $ x : ""

bin2int :: [Int] -> Int
bin2int = foldl (\acc x -> (acc * 2) + x) 0

countOnesPerPosition :: [String] -> [Int]
countOnesPerPosition = foldl aux []
    where
        aux acc [] = acc
        aux [] bs = aux [0] bs
        aux (a:as) (b:bs) = (a + (char2int b)) : (aux as bs)

filterMostCommon :: [String] -> [String]
filterMostCommon input = aux 0 input
    where
        aux _ [] = error "Got an empty array as an argument"
        aux _ (x:[]) = [x]
        aux toTake input = aux (toTake + 1) $ filter (\x -> (char2int $ value x) == mostCommon) input
            where
                value x = x !! toTake
                mostCommon = value $ getMostCommonBits input

filterLeastCommon :: [String] -> [String]
filterLeastCommon input = aux 0 input
    where
        aux _ [] = error "Got an empty array as an argument"
        aux _ (x:[]) = [x]
        aux toTake input = aux (toTake + 1) $ filter (\x -> (char2int $ value x) == leastCommon) input
            where
                value x = x !! toTake
                leastCommon = value $ getLeastCommonBits input

getMostCommonBits :: [String] -> [Int]
getMostCommonBits input = map (fromEnum . isMajority) $ countOnesPerPosition input
    where
        isMajority onesCount = (onesCount >= (length input) - onesCount)

getLeastCommonBits :: [String] -> [Int]
getLeastCommonBits input = map (fromEnum . isMinority) $ countOnesPerPosition input
    where
        isMinority onesCount = (onesCount < (length input) - onesCount)

getGamma :: [String] -> [Int]
getGamma = getMostCommonBits

getEpsilon :: [String] -> [Int]
getEpsilon = map (fromEnum . (/= 1)) . getGamma

getOxygen :: [String] -> [Int]
getOxygen = countOnesPerPosition . filterMostCommon

getCO2 :: [String] -> [Int]
getCO2 = countOnesPerPosition . filterLeastCommon

solve1 :: [String] -> Int
solve1 input = (bin2int $ getEpsilon input) * (bin2int $ getGamma input)

solve2 :: [String] -> Int
solve2 input = (bin2int $ getOxygen input) * (bin2int $ getCO2 input)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let input =  lines contents
    putStrLn $ "Part 1: " ++ (show $ solve1 input)
    putStrLn $ "Part 2: " ++ (show $ solve2 input)
