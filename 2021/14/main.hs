import Data.List
import qualified Data.Map as Map
import Data.Char

type Rules = [(String, Char)]
type PairFreq = Map.Map String Int
type Polymer = String

step :: PairFreq -> Rules -> PairFreq
step polymer rules = foldl' updateFrequency polymer rules
  where
    updateFrequency acc (key@(k1:k2:[]), val)
      | Map.member key acc = addPair1 $ addPair2 acc
      | otherwise = acc
      where
        pair1 = val : k2 : []
        pair2 = k1 : val : []
        addPair1 = Map.insertWith (+) pair1 1
        addPair2 = Map.insertWith (+) pair2 1

-- TODO: Something seems wrong here, the numbers aren't adding up like they should
stepX :: Int -> PairFreq -> Rules -> PairFreq
stepX steps polymer rules
  | steps > 0 = stepX (steps - 1) newPolymer rules
  | otherwise = newPolymer
  where
    newPolymer = step polymer rules

-- TODO: This needs to be changed. Need to use the new maps and make sure to
-- count for each character in the key
getPolymerCounts :: Polymer -> [(Char, Int)]
getPolymerCounts = Map.toList . foldl' (\acc char -> Map.insertWith (+) char 1 acc) Map.empty

input2pairFreq :: Polymer -> PairFreq -> PairFreq
input2pairFreq (_:[]) freq = freq
input2pairFreq polymer@(_:p2:rest) freq = input2pairFreq (p2:rest) (Map.insertWith (+) (take 2 polymer) 1 freq)

solve :: Int -> Polymer -> Rules -> String
solve count polymer rules = show $ stepResult
  where
    initPairFreq = input2pairFreq polymer Map.empty
    stepResult = stepX count initPairFreq rules
    -- sortedCounts = sortOn snd $ getPolymerCounts stepResult
    -- bigger = snd $ last sortedCounts
    -- lower = snd $ head sortedCounts

-- solve :: Int -> Polymer -> Rules -> String
-- solve count polymer rules = show $ stepResult
--   where
--     stepResult = stepX count polymer rules

solve1 :: Polymer -> Rules -> String
solve1 = solve 10

-- solve2 :: Polymer -> Rules -> String
-- solve2 = solve 25

main :: IO ()
main = do
   contents <- readFile "input.txt"
   let input = lines contents
   let polymer = head input
   let rules = map ((\(x:_:y:[]) -> (x, y !! 0)) . words) $ drop 2 input
   putStrLn $ "Part 1: " ++ (solve1 polymer rules)
   -- putStrLn $ "Part 2: " ++ (solve2 polymer rules)
