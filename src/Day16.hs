module Day16
  ( day16,
  )
where

import Data.Attoparsec.Text
import qualified Data.List as L
import qualified Data.Text as T
import Relude

toDecimal :: [Char] -> Integer
toDecimal = foldl' foldBit 0
  where
    foldBit result '0' = result * 2
    foldBit result '1' = result * 2 + 1

toBoolean :: Char -> Bool
toBoolean '0' = False
toBoolean '1' = True

data PacketPayload = PPLiteral Integer | PPOperator [Packet]
  deriving (Eq, Show)

data Packet = Packet {_pVersion :: Integer, _pType :: Integer, _pPayload :: PacketPayload}
  deriving (Eq, Show)

bit :: Parser Char
bit = satisfy (inClass "01")

parseLiteral :: Parser PacketPayload
parseLiteral = PPLiteral . toDecimal <$> parseBlocks
  where
    parseBlocks = do
      more <- toBoolean <$> bit
      value <- count 4 bit
      if not more
        then return value
        else do
          rest <- parseBlocks
          return $ value ++ rest

parseOperator :: Parser PacketPayload
parseOperator = do
  mode <- toDecimal <$> count 1 bit
  if mode == 0
    then do
      subPacketSize <- toDecimal <$> count 15 bit
      subPacketBits <- count (fromInteger subPacketSize) bit
      case parseOnly (many1 parsePacket) (fromString subPacketBits) of
        Right packets -> return $ PPOperator packets
        Left err -> error $ toText err
    else do
      subPacketCount <- toDecimal <$> count 11 bit
      PPOperator <$> count (fromInteger subPacketCount) parsePacket

parsePacket :: Parser Packet
parsePacket = do
  pVersion <- toDecimal <$> count 3 bit
  pType <- toDecimal <$> count 3 bit
  pPayload <- if pType == 4 then parseLiteral else parseOperator
  return $ Packet {_pVersion = pVersion, _pType = pType, _pPayload = pPayload}

parseInput :: Text -> Packet
parseInput input = case parseOnly parsePacket (T.concatMap hexToBits input) of
  Right packet -> packet
  Left err -> error $ toText err
  where
    hexToBits '0' = "0000"
    hexToBits '1' = "0001"
    hexToBits '2' = "0010"
    hexToBits '3' = "0011"
    hexToBits '4' = "0100"
    hexToBits '5' = "0101"
    hexToBits '6' = "0110"
    hexToBits '7' = "0111"
    hexToBits '8' = "1000"
    hexToBits '9' = "1001"
    hexToBits 'A' = "1010"
    hexToBits 'B' = "1011"
    hexToBits 'C' = "1100"
    hexToBits 'D' = "1101"
    hexToBits 'E' = "1110"
    hexToBits 'F' = "1111"

versionSum :: Packet -> Integer
versionSum packet = _pVersion packet + payloadVersionSum (_pPayload packet)
  where
    payloadVersionSum (PPLiteral _) = 0
    payloadVersionSum (PPOperator packets) = sum (versionSum <$> packets)

evaluate :: Packet -> Integer
evaluate packet =
  case _pPayload packet of
    PPLiteral value -> value
    PPOperator packets ->
      let values = evaluate <$> packets
       in case _pType packet of
            0 -> sum values
            1 -> product values
            2 -> L.minimum values
            3 -> L.maximum values
            5 ->
              let (lhs : rhs : []) = values
               in if lhs > rhs then 1 else 0
            6 ->
              let (lhs : rhs : []) = values
               in if lhs < rhs then 1 else 0
            7 ->
              let (lhs : rhs : []) = values
               in if lhs == rhs then 1 else 0

day16 :: Text -> IO (String, String)
day16 input = do
  let packet = parseInput input

  return (show $ versionSum packet, show $ evaluate packet)
