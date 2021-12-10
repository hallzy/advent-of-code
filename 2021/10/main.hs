import Data.Maybe
import Data.List

openingOf :: Char -> Char
openingOf ')' = '('
openingOf ']' = '['
openingOf '}' = '{'
openingOf '>' = '<'

closingOf :: Char -> Char
closingOf '(' = ')'
closingOf '[' = ']'
closingOf '{' = '}'
closingOf '<' = '>'

corruptScore :: Char -> Int
corruptScore ')' = 3
corruptScore ']' = 57
corruptScore '}' = 1197
corruptScore '>' = 25137
corruptScore _ = 0

incompleteScore :: Char -> Int
incompleteScore ')' = 1
incompleteScore ']' = 2
incompleteScore '}' = 3
incompleteScore '>' = 4
incompleteScore _   = 0

syntaxCheck :: String -> (String, Maybe Char)
syntaxCheck = aux ""
   where
      aux :: String -> String -> (String, Maybe Char)
      aux stack [] = (stack, Nothing)
      aux stack (char:rest)
         | isOpening = aux (char : stack) rest
         | (openingOf char) == head stack = aux (tail stack) rest
         | otherwise = (stack, Just char)
         where
            isOpening = char `elem` "([{<"

solve1 :: [String] -> String
solve1 = show . sum . map (corruptScore . fromMaybe '_' . snd) . map syntaxCheck

solve2 :: [String] -> String
solve2 input = show $ solArr !! targetIdx
   where
      solArr = sort $ map (foldl (\acc char -> (acc * 5) + (incompleteScore char)) 0 . map closingOf) $ map fst $ filter ((== Nothing) . snd) $ map syntaxCheck input
      targetIdx = length solArr `div` 2

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let input = lines contents
    putStrLn $ "Part 1: " ++ (solve1 input)
    putStrLn $ "Part 2: " ++ (solve2 input)
