import Data.List
import Debug.Trace

nextSquare :: Int -> Int -> Int
nextSquare roll currentSquare = (roll + currentSquare) `mod` 10

player1Rolls :: [Int]
player1Rolls = zipWith3 (\a b c -> a + b + c) [1,7..] [2,8..] [3,9..]

player2Rolls :: [Int]
player2Rolls = zipWith3 (\a b c -> a + b + c) [4,10..] [5,11..] [6,12..]

playPlayerPart1 :: Int -> [Int] -> [Int]
playPlayerPart1 startingSquare rolls = aux rolls startingSquare 0
   where
      aux (r:rs) currentSquare currentScore = newScore : aux rs newSquare newScore
         where
            newSquare = nextSquare r currentSquare
            newScore = currentScore + newSquare

part2Rolls :: [(Int, Int, Int)]
part2Rolls = map (\gr -> (fst $ head gr, snd $ head gr, length gr)) $ group $ sort [(x,y) | x <- possibleCombos, y <- possibleCombos]
   where
      possibleCombos = [x + y + z | x <- [1..3], y <- [1..3], z <- [1..3]]

playPlayerPart2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
playPlayerPart2 startPos startScores = foldl' (\(a1, a2) (x,y) -> (a1 + x, a2 + y)) (0,0) $ map (aux startPos startScores) part2Rolls
-- playPlayerPart2 startPos startScores = trace (show (startPos, startScores)) $ foldl' (\(a1, a2) (x,y) -> (a1 + x, a2 + y)) (0,0) $ map (aux startPos startScores) part2Rolls

aux :: (Int, Int) -> (Int, Int) -> (Int, Int, Int) -> (Int, Int)
aux pos@(player1Pos, player2Pos) scores@(player1Score, player2Score) (p1Roll, p2Roll, occurrences)
   | player1Score >= 15 = (occurrences, 0)
   | player2Score >= 15 = (0, occurrences)
   | otherwise = playPlayerPart2 (nextPlayer1, nextPlayer2) (nextScore1, nextScore2)
   where
      nextPlayer1 = nextSquare p1Roll player1Pos
      nextPlayer2 = nextSquare p2Roll player2Pos

      nextScore1 = player1Score + nextPlayer1
      nextScore2 = player2Score + nextPlayer2


solve1 :: [Int] -> String
solve1 (player1Start : player2Start : []) = show (losingScore * numRolls)
   where
      player1Play = takeWhile (< 1000) $ playPlayerPart1 player1Start player1Rolls
      player2Play = takeWhile (< 1000) $ playPlayerPart1 player2Start player2Rolls

      player1PlayLength = length player1Play
      player2PlayLength = length player2Play

      player1Won = min == player1PlayLength

      min = minimum [player1PlayLength, player2PlayLength]

      losingScore = head $ if player1Won then drop (min - 1) player2Play else drop min player1Play

      numRolls = (min * 3 * 2) + (if player1Won then 3 else 6)

solve2 :: [Int] -> String
solve2 (player1Start : player2Start : []) = show $ playPlayerPart2 (player1Start, player2Start) (0, 0)

main :: IO ()
main = do
   contents <- readFile "input.txt"
   -- let input = map (read . drop 28) $ lines contents
   -- putStrLn $ "Test Part 1: " ++ (solve1 [4,8])
   -- putStrLn $ "Part 1:      " ++ (solve1 input)
   putStrLn $ "Test Part 2: " ++ (solve2 [4,8])
   -- putStrLn $ "Part 2:      " ++ (solve2 input)
