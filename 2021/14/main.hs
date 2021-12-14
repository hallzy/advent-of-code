import Data.List
import qualified Data.Map as Map

type Rules = [(String, Char)]
type PairFreq = Map.Map String Int
type CharFreq = Map.Map Char Int
type Polymer = String

step :: PairFreq -> Rules -> PairFreq
step polymer rules = foldl' updateFrequency polymer rules
  where
    updateFrequency acc (key@(k1:k2:[]), val)
      | Map.member key polymer = resetKey $ addPair1 $ addPair2 acc
      | otherwise = acc
      where
        pair1 = val : k2 : []
        pair2 = k1 : val : []
        keyCount = polymer Map.! key
        addPair1 = Map.insertWith (+) pair1 keyCount
        addPair2 = Map.insertWith (+) pair2 keyCount
        resetKey = Map.insertWith (flip (-)) key keyCount

stepX :: Int -> PairFreq -> Rules -> PairFreq
stepX steps polymer rules
  | steps > 0 = stepX (steps - 1) newPolymer rules
  | otherwise = polymer
  where
    newPolymer = step polymer rules

input2pairFreq :: Polymer -> PairFreq -> PairFreq
input2pairFreq (_:[]) freq = freq
input2pairFreq polymer@(_:p2:rest) freq = input2pairFreq (p2:rest) (Map.insertWith (+) (take 2 polymer) 1 freq)

countChars :: PairFreq -> CharFreq
countChars pairs = Map.foldlWithKey (\acc (c1:c2:[]) count -> Map.insertWith (+) c2 count (Map.insertWith (+) c1 count acc)) Map.empty pairs

makeEven :: Int -> Int
makeEven val
  | val `mod` 2 == 0 = val
  | otherwise        = val + 1

solve :: Int -> Polymer -> Rules -> String
solve count polymer rules = show $ bigger - lower
  where
    initPairFreq = input2pairFreq polymer Map.empty
    stepResult = stepX count initPairFreq rules
    counts = Map.map (\value -> (makeEven value) `div` 2) $ countChars stepResult
    countsList = map (\(_, count) -> count) $ Map.toList counts
    bigger = maximum countsList
    lower = minimum countsList

solve1 :: Polymer -> Rules -> String
solve1 = solve 10

solve2 :: Polymer -> Rules -> String
solve2 = solve 40

main :: IO ()
main = do
   contents <- readFile "input.txt"
   let input = lines contents
   let polymer = head input
   let rules = map ((\(x:_:y:[]) -> (x, y !! 0)) . words) $ drop 2 input
   putStrLn $ "Part 1: " ++ (solve1 polymer rules)
   putStrLn $ "Part 2: " ++ (solve2 polymer rules)
