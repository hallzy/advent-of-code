import Data.List
import Control.Applicative

type Span = (Int, Int)
type Target = (Span, Span)

split :: String -> String -> [String]
split delim str = aux "" delim str
   where
      aux :: String -> String -> String -> [String]
      aux currentString delim str
         | str == "" = currentString : []
         | take (length delim) str == delim = currentString : (aux "" delim (drop (length delim) str))
         | otherwise = aux (currentString ++ (take 1 str)) delim (tail str)

validVelocities :: Target -> [(Int, Int)]
validVelocities target = filter (fst . velocityHitsTarget target) velocitiesShortlist
   where
      velocitiesShortlist = liftA2 (,) (validXVelocities target) (validYVelocities target)

maxYPos :: Target -> Int
maxYPos target = maximum $ map (snd . velocityHitsTarget target) velocitiesShortlist
   where
      velocitiesShortlist = liftA2 (,) (validXVelocities target) (validYVelocities target)

velocityHitsTarget :: Target -> (Int, Int) -> (Bool, Int)
velocityHitsTarget ((xStart, xEnd), (yStart, yEnd)) velocity = aux (0, 0) velocity 0
   where
      aux :: (Int, Int) -> (Int, Int) -> Int -> (Bool, Int)
      aux (xPos, yPos) (xVelocity, yVelocity) maxYPos
         | xPos > xEnd || yPos < yEnd = (False, 0)
         | xPos >= xStart && yPos <= yStart = (True, newMaxYPos)
         | otherwise = aux (xPos + xVelocity, yPos + yVelocity) (maximum [0, xVelocity - 1], yVelocity - 1) newMaxYPos
         where
            newMaxYPos = maximum [maxYPos, yPos]

validXVelocities :: Target -> [Int]
validXVelocities target@((_, xEnd), _) = filter (flip xVelocityHitsTarget target) [1..xEnd]

xVelocityHitsTarget :: Int -> Target -> Bool
xVelocityHitsTarget velocity ((targetStart, targetEnd), _) = aux 0 velocity
   where
      aux xPos velocity
         | xPos > targetEnd = False
         | xPos >= targetStart = True
         | velocity == 0 = False
         | otherwise = aux (xPos + velocity) (velocity - 1)

validYVelocities :: Target -> [Int]
validYVelocities target@(_, (_, yEnd)) = map (\(velocity, _) -> velocity) $ filter (\(_, bool) -> bool)  $ map (\velocity -> (velocity, yVelocityHitsTarget velocity target)) [yEnd..((-1) * yEnd)]

yVelocityHitsTarget :: Int -> Target -> Bool
yVelocityHitsTarget velocity (_, (targetStart, targetEnd)) = aux 0 velocity
   where
      aux yPos velocity
         | yPos < targetEnd = False
         | yPos <= targetStart = True
         | otherwise = aux (yPos + velocity) (velocity - 1)

parseInput :: String -> Target
parseInput input = ((xStart, xEnd), (yStart, yEnd))
   where
      cleanerInput :: [Int]
      cleanerInput = concatMap (map read . split ".." . drop 2) $ split ", " $ drop 13 input

      (xStart:xEnd:yEnd:yStart:[]) = cleanerInput

solve1 :: Target -> String
solve1 = show . maxYPos

solve2 :: Target -> String
solve2 = show . length . validVelocities

main :: IO ()
main = do
   contents <- readFile "input.txt"
   let input = contents
   putStrLn $ "Part 1: " ++ (solve1 (parseInput input))
   putStrLn $ "Part 2: " ++ (solve2 (parseInput input))
