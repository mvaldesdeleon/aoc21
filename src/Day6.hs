{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Day6
  ( day6,
  )
where

import Data.Attoparsec.Text
import Data.List ((!!))
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Relude

newtype Lanternfish a = Lanternfish a
  deriving (Show, Eq, Ord, Num)

parseLanternfish :: Parser a -> Parser (Lanternfish a)
parseLanternfish pa = Lanternfish <$> pa

parseInput :: Text -> [Lanternfish Integer]
parseInput input =
  case parseOnly (parseLanternfish decimal `sepBy1` char ',') input of
    Right ls -> ls
    Left err -> error $ fromString err

generation :: (Eq a, Num a) => [Lanternfish a] -> [Lanternfish a]
generation school = go school 0
  where
    go (l : ls) spawn = cycle l : go ls (if l == 0 then spawn + 1 else spawn)
    go [] spawn = replicate spawn 8
    cycle l = if l == 0 then 6 else l - 1

buildFloor :: Ord a => [Lanternfish a] -> M.Map (Lanternfish a) Integer
buildFloor school = foldl' insertLanternfish M.empty (L.group . L.sort $ school)
  where
    insertLanternfish floor (l : ls) = M.insert l ((toInteger . length $ ls) + 1) floor
    insertLanternfish _ [] = error $ "Data.List.group is broken"

generation' :: (Ord a, Num a) => M.Map (Lanternfish a) Integer -> M.Map (Lanternfish a) Integer
generation' = M.foldlWithKey' insertLanternfish M.empty
  where
    insertLanternfish floor l count =
      if l == 0
        then M.insertWith (+) 8 count (M.insertWith (+) 6 count floor)
        else M.insertWith (+) (l - 1) count floor

countLanternfish :: M.Map (Lanternfish a) Integer -> Integer
countLanternfish = M.foldl' (+) 0

day6 :: Text -> IO (String, String)
day6 input =
  let school = parseInput input
      part1 = iterate generation school !! 80
      part2 = countLanternfish $ iterate generation' (buildFloor school) !! 256
   in return (show $ length part1, show $ part2)
