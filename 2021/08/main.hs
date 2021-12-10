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
uniqueFind len (x:xs)
    | length x == len = x
    | otherwise          = uniqueFind len xs

permuteFind :: Int -> String -> [String] -> String
permuteFind _ _ [] = error "idk"
permuteFind len toPermute (x:xs)
    | length x == len && (all (\p -> p `elem` x) toPermute) = x
    | otherwise = permuteFind len toPermute xs

getFive :: String -> String -> String -> String
getFive primary secondary four
   | count == 3 = primary
   | otherwise = secondary
   where
      count = length $ filter ((== 2) . length) $ group $ sort $ primary ++ four

dropStrings :: [String] -> [String] -> [String]
dropStrings _ [] = []
dropStrings toDrop (x:xs)
   | x `elem` toDrop = dropStrings toDrop xs
   | otherwise = x : dropStrings toDrop xs

find' :: [String] -> [(Char, [String])]
find' xs = do
   -- 1, 4, 7, and 8 all have unique lengths, so they can be easily deduced
   -- based on the length of the string
   let one   = uniqueFind 2 xs
   let four  = uniqueFind 4 xs
   let seven = uniqueFind 3 xs
   let eight = uniqueFind 7 xs

   let xs2 = dropStrings [one, four, seven, eight] xs

   -- 9 uses 6 segments, and is the only 6 segment value that shares all the
   -- same segments as a 4
   let nine = permuteFind 6 four xs2
   let xs3 = dropStrings [nine] xs2

   -- 0 is a 6 segment value and after figuring out 9, it is the only 6 segment
   -- value to share all the segments of a 1
   let zero = permuteFind 6 one xs3
   let xs4 = dropStrings [zero] xs3

   -- 6 is a 6 segment value and is the only 6 segment value remaining after 9
   -- and 0
   let six = uniqueFind 6 xs4
   let xs5 = dropStrings [six] xs4

   -- 3 is a 5 segment value and is the only 5 segment value that shares the
   -- segments of a 1
   let three = permuteFind 5 one xs5
   let xs6 = dropStrings [three] xs5

   -- 5 is a 5 segment value and is the only 5 seg value that has all the
   -- segments from a 4 except for 1
   let five = getFive (head xs6) (last xs6) four
   let xs7 = dropStrings [five] xs6

   -- 2 is the only value left
   let two = head xs7

   [
      ('0', permutations zero),
      ('1', permutations one),
      ('2', permutations two),
      ('3', permutations three),
      ('4', permutations four),
      ('5', permutations five),
      ('6', permutations six),
      ('7', permutations seven),
      ('8', permutations eight),
      ('9', permutations nine)
      ]

decode :: [String] -> [(Char, [String])] -> Int
decode outputs decoder = read $ aux outputs
   where
      aux :: [String] -> [Char]
      aux [] = []
      aux (output:outputs) = (fst $ head $ filter (\x -> output `elem` (snd x)) decoder) : (aux outputs)

solve1 :: [[[String]]] -> Int
solve1 [] = 0
solve1 (x:xs) = (aux x) + (solve1 xs)
    where
        aux (_:output:[]) = (length $ filter isUniqueSize output)
        aux _ = error "Invalid input"

solve2 :: [[[String]]] -> Int
solve2 [] = 0
solve2 ((input:output:[]):xs) = (decode output (find' input)) + solve2 xs

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let input = map (map (words . trim) . split '|') $ lines contents
    putStrLn $ "Part 1: " ++ (show $ solve1 input)
    putStrLn $ "Part 2: " ++ (show $ solve2 input)
