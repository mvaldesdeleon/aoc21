module Day3
  ( day3,
  )
where

import Control.Lens
import Data.Char (digitToInt)
import Numeric (readInt)
import Relude

parseInput :: Text -> [String]
parseInput input = map toString $ lines input

countBits :: Num a => String -> (a, a)
countBits [] = (0, 0)
countBits (b : bs) =
  case b of
    '0' -> countBits bs & _1 +~ 1
    '1' -> countBits bs & _2 +~ 1
    b -> error $ "Unexpected bit value: " <> one b

mostFrequent :: Ord a => (a, a) -> Char
mostFrequent (zeroes, ones) = if zeroes > ones then '0' else '1'

gammaRate :: [String] -> String
gammaRate report = map (mostFrequent . countBits) (transpose report)

complement :: String -> String
complement = map flip
  where
    flip '0' = '1'
    flip '1' = '0'
    flip b = error $ "Unexpected bit value: " <> one b

readBin :: Integral a => String -> a
readBin number =
  case fmap fst . listToMaybe . readInt 2 (`elem` ("01" :: String)) digitToInt $ number of
    Just dec -> dec
    Nothing -> error $ "Unable to parse binary number: " <> toText number

day3 :: Text -> IO (String, String)
day3 input = do
  let report = parseInput input
      gammaBin = gammaRate report
      epsilonBin = complement gammaBin
  return (show $ readBin gammaBin * readBin epsilonBin, "N/A")