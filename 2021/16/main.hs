import Data.List

type Version = Int
type TypeID = Int
type LiteralVal = Int
type LengthTypeID = Int
type BitsUsed = Int

type LiteralPacket = (Version, TypeID, LiteralVal, BitsUsed)

type OperatorPacket = (Version, TypeID, LengthTypeID, BitsUsed, [Packet])

data Packet = LiteralPacket LiteralPacket | OperatorPacket OperatorPacket deriving Show

char2str :: Char -> String
char2str = (: [])

bin2int :: String -> Int
bin2int bin = aux 0 (reverse bin)
   where
      aux :: Int -> String -> Int
      aux _ [] = 0
      aux exp (b:bs) = ((read $ char2str b) * (2 ^ exp)) + (aux (exp + 1) bs)

hex2binChar :: Char -> String
hex2binChar '0' = "0000"
hex2binChar '1' = "0001"
hex2binChar '2' = "0010"
hex2binChar '3' = "0011"
hex2binChar '4' = "0100"
hex2binChar '5' = "0101"
hex2binChar '6' = "0110"
hex2binChar '7' = "0111"
hex2binChar '8' = "1000"
hex2binChar '9' = "1001"
hex2binChar 'A' = "1010"
hex2binChar 'B' = "1011"
hex2binChar 'C' = "1100"
hex2binChar 'D' = "1101"
hex2binChar 'E' = "1110"
hex2binChar 'F' = "1111"
hex2binChar _   = ""

hex2bin :: String -> String
hex2bin = concatMap hex2binChar

getLiteralPacket :: String -> LiteralPacket
getLiteralPacket str
   | typeID /= 4 = (-1, -1, -1, 0)
   | otherwise = (version, typeID, literal, bitsUsed)
   where
      (version, typeID, remaining) = getCommonStuff str

      literalStr = getLiteral remaining
      len = length literalStr
      literal = bin2int literalStr

      bitsUsed = 6 + (len + (len `div` 4))

      getLiteral :: String -> String
      getLiteral remaining
         | isLast = bits
         | otherwise = bits ++ (getLiteral rest)
         where
            isLast = (take 1 remaining) == "0"
            bits = take 4 $ drop 1 remaining

            rest = drop 5 remaining

getOperatorPacket :: String -> OperatorPacket
getOperatorPacket "" = error "Trying to get an operator packet from an empty string"
getOperatorPacket str
   | typeID == 4 = (-1, -1, -1, 0, [])
   | lengthTypeID == 0 = (version, typeID, lengthTypeID, bitsUsed15, literals15)
   | otherwise = (version, typeID, lengthTypeID, bitsUsed11, literals11)
   where
      (version, typeID, remaining) = getCommonStuff str

      lengthTypeID = bin2int $ take 1 remaining
      leftOver1 = drop 1 remaining

      subpacketsLengthLength = if lengthTypeID == 1 then 11 else 15

      -- For lengthTypeID == 0
      subpacketsLength15 = bin2int $ take 15 leftOver1
      leftOver15_2 = drop 15 leftOver1
      subpackets15 = take subpacketsLength15 leftOver15_2
      leftOver15_3 = drop subpacketsLength15 leftOver15_2
      bitsUsed15 = 6 + 1 + 15 + (sum $ getBitsUsed literals15)
      literals15 = parse (-1) True subpackets15

      -- For lengthTypeID == 1
      numSubpackets11 = bin2int $ take 11 leftOver1
      leftOver11_2 = drop 11 leftOver1
      bitsUsed11 = 6 + 1 + 11 + (sum $ getBitsUsed literals11)
      literals11 = parse numSubpackets11 True leftOver11_2

getBitsUsed :: [Packet] -> [BitsUsed]
getBitsUsed [] = []
getBitsUsed ((LiteralPacket (_, _, _, used)):packets) = used : getBitsUsed packets
getBitsUsed ((OperatorPacket (_, _, _, used, _)):packets) = used : getBitsUsed packets

getVersions :: [Packet] -> [Version]
getVersions [] = []
getVersions ((LiteralPacket (version, _, _, _)):packets) = version : getVersions packets
getVersions ((OperatorPacket (version, _, _, _, subPackets)):packets) = version : getVersions (subPackets ++ packets)

getCommonStuff :: String -> (Version, TypeID, String)
getCommonStuff str = (version, typeID, remainingString)
   where
      version = bin2int $ take 3 str
      typeID = bin2int $ take 3 $ drop 3 str
      remainingString = drop 6 str

operate :: [Packet] -> [Int]
operate [] = []
operate (LiteralPacket (_, _, value, _):packets) = value : operate packets
operate (OperatorPacket (_, operatorType, _, _, subPackets):packets)
   | isSum     = sum (operate subPackets) : operate packets
   | isProduct = product (operate subPackets) : operate packets
   | isMinimum = minimum (operate subPackets) : operate packets
   | isMaximum = maximum (operate subPackets) : operate packets
   | isGT      = (if (operate $ [head subPackets]) > (operate $ [last subPackets]) then 1 else 0) : operate packets
   | isLT      = (if (operate $ [head subPackets]) < (operate $ [last subPackets]) then 1 else 0) : operate packets
   | isEQ      = (if (operate $ [head subPackets]) == (operate $ [last subPackets]) then 1 else 0) : operate packets
   | otherwise = error $ "Invalid Operator Type: " ++ (show operatorType)
   where
      isSum     = operatorType == 0
      isProduct = operatorType == 1
      isMinimum = operatorType == 2
      isMaximum = operatorType == 3
      -- Operator Type 4 is a literal value, so ignore
      isGT      = operatorType == 5
      isLT      = operatorType == 6
      isEQ      = operatorType == 7

roundUpToNearestMultipleOf8 val = 8 - (val `mod` 8)

parse :: Int -> Bool -> String -> [Packet]
parse count isInsideOperator bin = aux count bin
   where
      aux :: Int -> String -> [Packet]
      aux count bin
         | bin == []        = []
         | count == 0       = []
         | isLiteralPacket  = (LiteralPacket literalPacket) : (aux (count - 1) (drop (literalTotalBitsUsed + literalBitsLeftUnused) bin))
         | otherwise        = (OperatorPacket operatorPacket) : (aux (count -1) (drop (operatorTotalBitsUsed + operatorBitsLeftUnused) bin))
         where
            (_, typeID, _) = getCommonStuff bin

            isLiteralPacket = typeID == 4

            literalPacket@(_, _, _, literalBitsUsed) = getLiteralPacket bin
            literalTotalBitsUsed = literalBitsUsed
            literalBitsLeftUnused = if isInsideOperator then 0 else roundUpToNearestMultipleOf8 literalTotalBitsUsed

            operatorPacket@(_, _, _, operatorBitsUsed, _) = getOperatorPacket bin
            operatorTotalBitsUsed = operatorBitsUsed
            operatorBitsLeftUnused = if isInsideOperator then 0 else roundUpToNearestMultipleOf8 operatorTotalBitsUsed

solve1 :: String -> String
solve1 = show . sum . getVersions . parse (-1) False . hex2bin

solve2 :: String -> String
solve2 = show . head . operate . parse (-1) False . hex2bin

main :: IO ()
main = do
   contents <- readFile "input.txt"
   let input = contents
   putStrLn $ "Part 1: " ++ (solve1 input)
   putStrLn $ "Part 2: " ++ (solve2 input)
