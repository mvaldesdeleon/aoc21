{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
-- Sue me
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day7
  ( day7,
  )
where

import Data.Attoparsec.Text
import qualified Data.List as L
import Relude

newtype Crab a = Crab {unCrab :: a}
  deriving (Show, Eq, Ord, Num)

parseCrab :: Parser a -> Parser (Crab a)
parseCrab pa = Crab <$> pa

parseInput :: Text -> [Crab Integer]
parseInput input =
  case parseOnly (parseCrab decimal `sepBy1` char ',') input of
    Right ls -> ls
    Left err -> error $ fromString err

data Summary a = Summary {_sPos :: a, _sCount :: Integer, _sRest :: Integer}
  deriving (Show)

buildSummary :: [[Crab a]] -> [Summary a]
buildSummary grouped =
  let remaining = genericLength . concat $ grouped
   in go 0 remaining grouped
  where
    go :: Integer -> Integer -> [[Crab a]] -> [Summary a]
    go _ _ [] = []
    go behind remaining (crabs : rest) =
      let Crab a : _ = crabs
          count = genericLength crabs
          behind' = behind + count
          remaining' = remaining - count
       in Summary a behind' remaining' : go behind' remaining' rest

findPosition :: Ord a => [Crab a] -> a
findPosition crabs =
  let Just sa = L.find (\Summary {_sCount, _sRest} -> _sCount > _sRest) . buildSummary . L.group . L.sort $ crabs
   in _sPos sa

calculateFuel :: Num a => [Crab a] -> a -> a
calculateFuel crabs a = sum $ map (abs . (a -) . unCrab) crabs

calculateFuel' :: Integral a => [Crab a] -> a -> a
calculateFuel' crabs a = sum $ map (scale . abs . (a -) . unCrab) crabs
  where
    scale n = n * (n + 1) `div` 2

day7 :: Text -> IO (String, String)
day7 input =
  let crabs = parseInput input
      position = findPosition crabs
      size = unCrab . L.maximum $ crabs
      part2 = L.minimum . map (calculateFuel' crabs) $ [0 .. size]
   in return (show $ calculateFuel crabs position, show $ part2)
