{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Day11
  ( day11,
  )
where

import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Char (digitToInt)
import Data.Ix
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Relude

data Octopus = Charging Int8 | Flashing
  deriving (Show)

parseOctopus :: Char -> Octopus
parseOctopus c
  | c `elem` ['0' .. '9'] = Charging $ fromIntegral (digitToInt c)
  | otherwise = error $ "Invalid character"

type Position = (Int8, Int8)

gridStart = (0, 0)

gridEnd = (9, 9)

newtype STGrid s i o = STGrid {unGrid :: STArray s i o}

runSTGrid :: (forall s. ST s (STGrid s i e)) -> Array i e
runSTGrid s = runSTArray (unGrid <$> s)

class Monad m => MGrid a m where
  newGrid :: Ix i => (i, i) -> [o] -> m (a i o)
  readGrid :: Ix i => a i o -> i -> m o
  writeGrid :: Ix i => a i o -> i -> o -> m ()

instance MGrid (STGrid s) (ST s) where
  newGrid (from, to) os = STGrid <$> newListArray (from, to) os
  readGrid (STGrid arr) i = readArray arr i
  writeGrid (STGrid arr) i o = writeArray arr i o

parseInput :: Text -> [Octopus]
parseInput = concatMap (map parseOctopus . toString) . words

makeGrid :: [Octopus] -> ST s (STGrid s Position Octopus)
makeGrid = newGrid (gridStart, gridEnd)

step :: STGrid s Position Octopus -> ST s Integer
step grid = do
  flashing <- incrementGrid grid
  resolve flashing
  cleanupGrid grid
  where
    resolve [] = return ()
    resolve ((y, x) : rest) = do
      flashing <- forM [(y - 1, x - 1), (y - 1, x), (y - 1, x + 1), (y, x - 1), (y, x + 1), (y + 1, x - 1), (y + 1, x), (y + 1, x + 1)] (increment grid)
      resolve (rest ++ catMaybes flashing)

cleanupGrid :: STGrid s Position Octopus -> ST s Integer
cleanupGrid grid = do
  flashing <- forM (range (gridStart, gridEnd)) $ \(y, x) -> do
    o <- readGrid grid (y, x)
    case o of
      Flashing -> writeGrid grid (y, x) (Charging 0) $> Just 1
      Charging c -> return Nothing
  return $ sum $ catMaybes flashing

incrementGrid :: STGrid s Position Octopus -> ST s [Position]
incrementGrid grid = do
  flashing <- forM (range (gridStart, gridEnd)) (increment grid)
  return $ catMaybes flashing

increment :: STGrid s Position Octopus -> Position -> ST s (Maybe Position)
increment grid (y, x)
  | inRange (gridStart, gridEnd) (y, x) = do
    o <- readGrid grid (y, x)
    case o of
      Flashing -> return Nothing
      Charging c ->
        if c >= 9
          then writeGrid grid (y, x) Flashing $> Just (y, x)
          else writeGrid grid (y, x) (Charging (c + 1)) $> Nothing
  | otherwise = return Nothing

countFlashes :: [Octopus] -> Integer -> [Integer]
countFlashes os steps = runST $ do
  grid <- makeGrid os
  replicateM (fromInteger steps) (step grid)

syncAfter :: [Octopus] -> Integer
syncAfter os = runST $ do
  grid <- makeGrid os
  stepUntilFlash grid 0
  where
    stepUntilFlash grid count = do
      flashing <- step grid
      if flashing == toInteger (rangeSize (gridStart, gridEnd))
        then return $ count + 1
        else stepUntilFlash grid (count + 1)

day11 :: Text -> IO (String, String)
day11 input =
  let octopi = parseInput input
   in return (show $ sum $ countFlashes octopi 100, show $ syncAfter octopi)
