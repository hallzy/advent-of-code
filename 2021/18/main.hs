import Data.List
import Data.Maybe

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

add :: String -> String -> String
add str1 str2 = reduce ("[" ++ str1 ++ "," ++ str2 ++ "]")

addList :: [String] -> String
addList = foldl1' (\acc val -> add acc val)

getNum :: Int -> String -> String
getNum num str = aux 0 False "" str
   where
      aux :: Int -> Bool -> String -> String -> String
      aux _ _ current "" = current
      aux foundNum inNum current (x:xs)
         | not inNum && isDigit x = aux (foundNum + 1) True (x : current) xs
         | isDigit x              = aux foundNum True (x : current) xs

         -- foundNum, but not numeric, so we are done
         | foundNum == num = reverse current

         | otherwise = aux foundNum False "" xs

replaceFirstNum :: String -> String -> String
replaceFirstNum toReplace str = aux False "" str
   where
      aux :: Bool -> String -> String -> String
      aux _ _ "" = ""
      aux foundNum current (x:xs)
         | isDigit x = aux True (x : current) xs

         -- foundNum, but not numeric, so we are done
         | foundNum = toReplace ++ (x : xs)

         | otherwise = x : aux False "" xs

getPair :: String -> Maybe (Int, Int, Int)
getPair str = aux 1 "" "" str
   where
      aux :: Int -> String -> String -> String -> Maybe (Int, Int, Int)
      aux nextNum first second (x:xs)
         | nextNum == 1 && length first > 0 && x == ','      = aux 2 first second xs
         | length first > 0 && length second > 0 && x == ']' = Just (str2int (reverse first), str2int (reverse second), (length str) - (length xs) - 1)
         | isDigit x                                         = aux nextNum newFirst newSecond xs
         | otherwise                                         = Nothing
         where
            newFirst = if nextNum == 1 then x : first else first
            newSecond = if nextNum == 2 then x : second else second

split :: String -> String
split str = aux "" str
   where
      aux _ "" = str
      aux before (x:xs)
         | isDigit x && num >= 10 = reverse before ++ "[" ++ show firstNum ++ "," ++ show secondNum ++ "]" ++ (drop ((length numStr) - 1) xs)
         | otherwise              = aux (x : before) xs
         where
            numStr = getNum 1 (x:xs)
            num = str2int numStr

            modded = num `mod` 2
            firstNum = num `div` 2
            secondNum = firstNum + modded

explode :: String -> String
explode str = aux 0 "" str
   where
      aux _ _ [] = str
      aux depth before (x:xs)
         | x == '[' = aux (depth + 1) (x : before) xs
         | x == ']' = aux (depth - 1) (x : before) xs
         | depth >= 5 && pair /= Nothing = newBefore ++ "0" ++ newAfter
         | otherwise = aux depth (x : before) xs
         where
            pair = getPair (x : xs)
            (val1, val2, valLength) = fromMaybe (-1, -1, -1) $ pair

            newXS = drop valLength xs

            foundVal1 = str2int $ reverse $ getNum 1 before
            newBefore = init $ reverse $ replaceFirstNum (reverse $ show (val1 + foundVal1)) before

            foundVal2 = str2int $ getNum 1 newXS
            newAfter = replaceFirstNum (show (val2 + foundVal2)) $ newXS

reduce :: String -> String
reduce str
   | explodeHasNoChange && splitHasNoChange = str
   | explodeHasNoChange                     = reduce splitResult
   | otherwise                              = reduce explodeResult
   where
      explodeResult = explode str
      splitResult = split str

      explodeHasNoChange = explodeResult == str
      splitHasNoChange = splitResult == str

isDigit :: Char -> Bool
isDigit = flip elem "0123456789"

char2str :: Char -> String
char2str = (:[])

str2int :: String -> Int
str2int = read

magnitudeCalc :: Int -> Int -> Int
magnitudeCalc x y = (3 * x) + (2 * y)

magnitude :: Tree Int -> Int
magnitude (Node (Leaf x) (Leaf y)) = magnitudeCalc x y
magnitude (Node (Leaf x) y)        = magnitudeCalc x (magnitude y)
magnitude (Node (x) (Leaf y))      = magnitudeCalc (magnitude x) y
magnitude (Node x y)               = magnitudeCalc (magnitude x) (magnitude y)

isNumeric :: String -> Bool
isNumeric = all isDigit

parse :: String -> Tree Int
parse input = aux 0 "" "" True (tail $ init input)
   where
      aux _ currentLeft currentRight _ [] = Node newLeft newRight
         where
            newLeft =  if (isNumeric currentLeft) then (Leaf (str2int currentLeft)) else (parse currentLeft)
            newRight = if (isNumeric currentRight) then (Leaf (str2int currentRight)) else (parse currentRight)

      aux depth currentLeft currentRight onLeft (x:xs)
         | x == '['               = aux (depth + 1) newLeft newRight onLeft xs
         | x == ']'               = aux (depth - 1) newLeft newRight onLeft xs
         | parsedOneSide          = aux depth currentLeft currentRight False xs
         | otherwise              = aux depth newLeft newRight onLeft xs
         where
            currentVal = if onLeft then currentLeft else currentRight
            newLeft = if onLeft then currentLeft ++ (char2str x) else currentLeft
            newRight = if onLeft then currentRight else currentRight ++ (char2str x)

            parsedOneSide = x == ',' && depth == 0
            isNum = isNumeric currentVal

allCombos :: Eq a => [a] -> [[a]]
allCombos xs = [[x,y] | x <- xs, y <- xs, x /= y]

solve1 :: [String] -> String
solve1 = show . magnitude . parse . addList

solve2 :: [String] -> String
solve2 = show . maximum . map (magnitude . parse . addList) . allCombos

main :: IO ()
main = do
   contents <- readFile "input.txt"
   let input = lines contents
   putStrLn $ "Part 1: " ++ (solve1 input)
   putStrLn $ "Part 2: " ++ (solve2 input)
