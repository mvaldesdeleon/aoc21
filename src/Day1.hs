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

day1 :: Text -> IO (String, String)
day1 input = do
  let sonar = parseInput input
  let trend = makeTrend sonar
  let part1 = count GT trend
  return (show part1, "N/A")