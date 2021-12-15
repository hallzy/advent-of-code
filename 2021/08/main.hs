import Data.List

--   0:      1:      2:      3:      4:
--  aaaa    ....    aaaa    aaaa    ....
-- b    c  .    c  .    c  .    c  b    c
-- b    c  .    c  .    c  .    c  b    c
--  ....    ....    dddd    dddd    dddd
-- e    f  .    f  e    .  .    f  .    f
-- e    f  .    f  e    .  .    f  .    f
--  gggg    ....    gggg    gggg    ....
--
--   5:      6:      7:      8:      9:
--  aaaa    aaaa    aaaa    aaaa    aaaa
-- b    .  b    .  .    c  b    c  b    c
-- b    .  b    .  .    c  b    c  b    c
--  dddd    dddd    ....    dddd    dddd
-- .    f  e    f  .    f  e    f  .    f
-- .    f  e    f  .    f  e    f  .    f
--  gggg    gggg    ....    gggg    gggg
--
-- 2 must be cf (1)
-- 3 must be acf (7)
-- 4 must be bdcf (4)
-- 5 must be acdeg or acdfg or abdfg (adg)
-- 6 must be abcefg or abdefg or abcdfg (abfg)
-- 7 must be abcdefg (0)

solve1 :: [[String]] -> String
solve1 = show . length . concatMap (filter (`elem` [2, 3, 4, 7]) . map length)

-- Get the chars that are common to all strings in the input list
common :: [String] -> String
common [x] = x
common (x:xs) = filter (`elem` x) $ common xs

solve2 :: [[String]] -> [[String]] -> String
solve2 inputs outputs = show $ sum $ map (aux) $ zip inputs outputs
   where
      aux :: ([String], [String]) -> Int
      aux (input, output) = read $ concatMap show $ map getValue output
         where
            get1 n = head $ filter ((== n). length) input
            get1' n = common $ map head $ group $ sort $ filter ((==n) . length) input

            cf   = get1 2
            acf  = get1 3
            bdcf = get1 4
            bd   = filter (`notElem` cf) bdcf
            adg  = get1' 5
            abfg = get1' 6

            a = head $ filter (`notElem` cf) acf
            b = head $ common [abfg, bd]
            c = head $ filter (/= f) cf
            d = head $ filter (/= b) bd
            e = head $ filter (`notElem` [a, b, c, d, f, g]) "abcdefg"
            f = head $ filter (`notElem` [a, b, g]) abfg
            g = head $ filter (/= a) $ common [abfg, adg]

            getValue :: String -> Int
            getValue out
              |  (sort out) == (sort [a, b, c, e, f, g])    = 0
              |  (sort out) == (sort [c, f])                = 1
              |  (sort out) == (sort [a, c, d, e, g])       = 2
              |  (sort out) == (sort [a, c, d, f, g])       = 3
              |  (sort out) == (sort [b, c, d, f])          = 4
              |  (sort out) == (sort [a, b, d, f, g])       = 5
              |  (sort out) == (sort [a, b, d, e, f, g])    = 6
              |  (sort out) == (sort [a, c, f])             = 7
              |  (sort out) == (sort [a, b, c, d, e, f, g]) = 8
              |  (sort out) == (sort [a, b, c, d, f, g])    = 9

main :: IO ()
main = do
   contents <- readFile "input.txt"
   let programInput = lines contents
   let inputs = map (take 10 . words) programInput
   let outputs = map (drop 11 . words) $ programInput
   putStrLn $ "Part 1: " ++ (solve1 outputs)
   putStrLn $ "Part 2: " ++ (solve2 inputs outputs)
