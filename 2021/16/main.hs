import Data.List
import Debug.Trace

-- First 3 Bits
type Version = Int
type TypeID = Int
type LiteralVal = Int
type LengthTypeID = Int

type LiteralValPacket = (Version, TypeID, LiteralVal)

type OperatorPacket = (Version, TypeID, LengthTypeID, [Packet])

data Packet = LiteralValPacket LiteralValPacket | OperatorPacket OperatorPacket deriving Show
-- type Packet = Either LiteralValPacket OperatorPacket

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

getLiteralPacket :: String -> (LiteralValPacket, Int)
getLiteralPacket str
   | typeID /= 4 = ((-1, -1, -1), 0)
   | otherwise = ((version, typeID, literal), bitsUsed)
   where
      (version, typeID, remaining) = getCommonStuff str

      literalStr = getLiteral remaining
      len = length literalStr
      literal = bin2int literalStr

      bitsUsed = (len + (len `div` 4))

      getLiteral :: String -> String
      getLiteral remaining
         | isLast = bits
         | otherwise = bits ++ (getLiteral rest)
         where
            isLast = (take 1 remaining) == "0"
            bits = take 4 $ drop 1 remaining

            rest = drop 5 remaining

-- type OperatorPacket = (Version, TypeID, LengthTypeID, [LiteralValPacket])
getOperatorPacket :: String -> (OperatorPacket, Int)
getOperatorPacket "" = error "wtf"
getOperatorPacket str
   | typeID == 4 = ((-1, -1, -1, []), 0)
   | otherwise = ((version, typeID, lengthTypeID, literals), bitsUsed)
   where
      (version, typeID, remaining) = getCommonStuff str

      lengthTypeID = bin2int $ take 1 remaining
      leftOver1 = drop 1 remaining

      -- TODO: 11 isn't actually the length of the subpackets length, it is the
      -- number of packets, so I need separate handling for 11
      subpacketsLengthLength = if lengthTypeID == 1 then 11 else 15

      subpacketsLength = bin2int $ take subpacketsLengthLength leftOver1

      leftOver2 = drop subpacketsLengthLength leftOver1

      subpackets = take subpacketsLength leftOver2

      leftOver3 = drop subpacketsLength leftOver2

      bitsUsed = 1 + (length leftOver1) - (length leftOver3)

      literals = parse True subpackets

getCommonStuff :: String -> (Version, TypeID, String)
getCommonStuff str = (version, typeID, remainingString)
   where
      version = bin2int $ take 3 str
      typeID = bin2int $ take 3 $ drop 3 str
      remainingString = drop 6 str

roundUpToNearestMultipleOf8 val = 8 - (val `mod` 8)

trace' val = trace (show val) val

parse :: Bool -> String -> [Packet]
parse isInsideOperator bin = aux bin
   where
      aux :: String -> [Packet]
      aux bin
         | bin == []        = []
         | isLiteralPacket  = (LiteralValPacket literalPacket) : (aux (drop (literalTotalBitsUsed + literalBitsLeftUnused) bin))
         | otherwise        = (OperatorPacket operatorPacket) : (aux (drop (operatorTotalBitsUsed + operatorBitsLeftUnused) bin))
         where
            (_, typeID, _) = getCommonStuff (trace' bin)

            isLiteralPacket = typeID == 4

            (literalPacket, literalBitsUsed) = getLiteralPacket bin
            literalTotalBitsUsed = literalBitsUsed + 6
            literalBitsLeftUnused = if isInsideOperator then 0 else roundUpToNearestMultipleOf8 literalTotalBitsUsed

            (operatorPacket, operatorBitsUsed) = getOperatorPacket bin
            operatorTotalBitsUsed = operatorBitsUsed + 6
            operatorBitsLeftUnused = roundUpToNearestMultipleOf8 operatorTotalBitsUsed

solve :: String -> String
solve = show . parse False . hex2bin

main :: IO ()
main = do
   contents <- readFile "input.txt"
   let input = contents
   putStrLn $ "Part 1: " ++ (solve input)
