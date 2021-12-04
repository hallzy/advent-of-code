import Data.List

type Draw = [Int]
type BingoRow = [Int]
type BingoCard = [[Int]]

winner :: Int -> BingoRow
winner size = take size $ repeat (-1)

findWinningCard :: [BingoCard] -> BingoCard
findWinningCard [] = []
findWinningCard (card:cards)
    | winningRows > 0 || winningColumns > 0 = card
    | otherwise = findWinningCard cards
    where
        size = length card
        winningRows = length $ filter (== (winner size)) card
        winningColumns = length $ filter (== (winner size)) $ transpose card

findCardsThatHaventWon :: [BingoCard] -> [BingoCard]
findCardsThatHaventWon [] = []
findCardsThatHaventWon (card:cards)
    | winningRows > 0 || winningColumns > 0 = findCardsThatHaventWon cards
    | otherwise = card : findCardsThatHaventWon cards
    where
        size = length card
        winningRows = length $ filter (== (winner size)) card
        winningColumns = length $ filter (== (winner size)) $ transpose card

score :: BingoCard -> Int
score = sum . map (sum . filter (/= -1))

parseBingoCards :: [String] -> [BingoCard]
parseBingoCards raw = aux $ listOfRows raw
    where
        listOfRows :: [String] -> [BingoRow]
        listOfRows raw = filter (/= []) $ map (map read . filter (/= "") . split ' ') raw

        cardSize :: Int
        cardSize = length $ head $ listOfRows raw

        aux :: [BingoRow] -> [BingoCard]
        aux [] = []
        aux rows = (take cardSize rows) : (aux $ drop cardSize rows)

split :: Char -> String -> [String]
split delim str = lines $ map aux str
    where
        aux item
            | item == delim = '\n'
            | otherwise = item

solve1 :: Draw -> [BingoCard] -> String
solve1 draw bingoCards = aux (-1) draw bingoCards
    where
        aux :: Int -> Draw -> [BingoCard] -> String
        aux prevDraw (d:ds) bingoCards
            | winningCard /= [] = show $ prevDraw * (score winningCard)
            | otherwise = aux d ds $ map (map (map (\x -> if x == d then -1 else x))) bingoCards
            where
                winningCard = findWinningCard bingoCards

solve2 :: Draw -> [BingoCard] -> String
solve2 draw bingoCards = aux (-1) draw bingoCards
    where
        aux :: Int -> Draw -> [BingoCard] -> String
        aux prevDraw (d:ds) bingoCards
            | (length cardsThatHaventWon) /= 1 = aux d ds $ map (map (map (\x -> if x == d then -1 else x))) bingoCards
            | otherwise = solve1 (d:ds) cardsThatHaventWon
            where
                cardsThatHaventWon = findCardsThatHaventWon bingoCards

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let input =  lines contents
    let drawn = map read $ split ',' $ head input
    let bingoCards = parseBingoCards $ drop 2 input
    putStrLn $ "Part 1: " ++ (solve1 drawn bingoCards)
    putStrLn $ "Part 2: " ++ (solve2 drawn bingoCards)
