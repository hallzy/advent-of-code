import Data.List

solve1 :: String -> String
solve1 = show

main :: IO ()
main = do
   contents <- readFile "input.txt"
   let input = contents
   putStrLn $ "Part 1: " ++ (solve1 input)
