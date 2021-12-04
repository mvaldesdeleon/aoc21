module Day1
  ( day1,
  )
where

import Data.Maybe (mapMaybe)
import Relude

newtype Sonar a = Sonar [a]

parseInput :: Text -> Sonar Integer
parseInput input = Sonar $ mapMaybe (readMaybe . toString) (lines input)

makeTrend :: Ord a => Sonar a -> Sonar Ordering
makeTrend (Sonar (a : as)) = Sonar $ zipWith compare as (a : as)
makeTrend (Sonar []) = Sonar []

count :: Eq a => a -> Sonar a -> Integer
count a (Sonar as) = toInteger . length $ filter (== a) as

slidingWindow :: Integer -> [a] -> [[a]]
slidingWindow n xs = transpose (take (fromInteger n) (tails xs))

threeMeasurement :: Num a => Sonar a -> Sonar a
threeMeasurement (Sonar as) = Sonar $ map sum (slidingWindow 3 as)

day1 :: Text -> IO (String, String)
day1 input =
  let sonar = parseInput input
      trend = makeTrend sonar
      part1 = count GT trend
      sonar' = threeMeasurement sonar
      trend' = makeTrend sonar'
      part2 = count GT trend'
   in return (show part1, show part2)