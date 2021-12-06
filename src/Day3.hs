module Day3
  ( day3,
  )
where

import Control.Lens
import Data.Char (digitToInt)
import Data.List ((!!))
import Numeric (readInt)
import Relude

newtype Binary = Binary String
  deriving (Show)

unpack :: Binary -> String
unpack (Binary number) = number

parseInput :: Text -> [Binary]
parseInput input = map (Binary . toString) $ lines input

countBits :: Num a => String -> (a, a)
countBits [] = (0, 0)
countBits (b : bs) =
  case b of
    '0' -> countBits bs & _1 +~ 1
    '1' -> countBits bs & _2 +~ 1
    b -> error $ "Unexpected bit value: " <> one b

mostFrequent :: Ord a => (a, a) -> Char
mostFrequent (zeroes, ones) = if zeroes > ones then '0' else '1'

gammaRate :: [Binary] -> Binary
gammaRate report = Binary $ map (mostFrequent . countBits) (transpose $ map unpack report)

complement :: Binary -> Binary
complement (Binary number) = Binary $ map flip number
  where
    flip '0' = '1'
    flip '1' = '0'
    flip b = error $ "Unexpected bit value: " <> one b

readBin :: Integral a => Binary -> a
readBin (Binary number) =
  case fmap fst . listToMaybe . readInt 2 (`elem` ("01" :: String)) digitToInt $ number of
    Just dec -> dec
    Nothing -> error $ "Unable to parse binary number: " <> toText number

refine :: Binary -> Integer -> [Binary] -> [Binary]
refine (Binary bitCriteria) bitPosition = filter match
  where
    match (Binary number) = number !! fromInteger bitPosition == bitCriteria !! fromInteger bitPosition

data Rating = O2Generator | CO2Scrubber
  deriving (Show)

findRating :: Rating -> [Binary] -> Binary
findRating rating report = go 0 report
  where
    go 20 _ = error "We should've stopped by now"
    go bitPosition report =
      let gammaBin = gammaRate report
          bitCriteria =
            case rating of
              O2Generator -> gammaBin
              CO2Scrubber -> complement gammaBin
       in case refine bitCriteria bitPosition report of
            [rating] -> rating
            [] -> error "Could not find rating"
            refined -> go (bitPosition + 1) refined

day3 :: Text -> IO (String, String)
day3 input =
  let report = parseInput input
      gammaBin = gammaRate report
      epsilonBin = complement gammaBin
      o2rating = findRating O2Generator report
      co2rating = findRating CO2Scrubber report
   in return (show $ readBin gammaBin * readBin epsilonBin, show $ readBin o2rating * readBin co2rating)