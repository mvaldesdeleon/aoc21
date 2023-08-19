module Day15
  ( day15,
  )
where

import Data.Char (digitToInt)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Text as T
import Relude

type Position = (Integer, Integer)

type Cave = M.Map Position Integer

parseInput :: Text -> Cave
parseInput input =
  let rows = lines input
      height = genericLength rows
      width = toInteger . T.length $ L.head rows
      coords = [(x, y) | x <- [1 .. width], y <- [1 .. height]]
   in M.fromAscList $ zip coords (concatMap digitsToIntegers rows)
  where
    digitsToIntegers = map (toInteger . digitToInt) . T.unpack

type Risks = M.Map Position Integer

type Candidates = PQ.MinPQueue Integer Position

findRisk :: Cave -> Position -> Integer
findRisk cave position =
  let risks = M.singleton (1, 1) 0
      candidates = PQ.singleton 0 (1, 1)
   in evalState (findPath cave position) (risks, candidates)

findPath :: Cave -> Position -> State (Risks, Candidates) Integer
findPath cave destination = do
  candidates <- gets snd
  let ((currentRisk, current), candidates') = PQ.deleteFindMin candidates
  if current == destination
    then return currentRisk
    else do
      modify (\(risks, _) -> (risks, candidates'))
      risks <- gets fst
      forM_ (filter (`M.notMember` risks) (neighbors current)) $ \neighbor -> do
        let risk = cave M.! neighbor
        modify (\(risks, candidates) -> (M.insert neighbor (currentRisk + risk) risks, PQ.insert (currentRisk + risk) neighbor candidates))
      findPath cave destination
  where
    (width, height) = fst . M.findMax $ cave
    neighbors (x, y) = filter (\(x, y) -> x >= 1 && y >= 1 && x <= width && y <= height) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

extendedCave :: Cave -> Cave
extendedCave cave =
  let (width, height) = fst . M.findMax $ cave
   in M.fromAscList [((x, y), (cave M.! (x `mod1` width, y `mod1` height) + x `div1` width + y `div1` height) `mod1` 9) | x <- [1 .. width * 5], y <- [1 .. height * 5]]
  where
    mod1 a b = ((a - 1) `mod` b) + 1
    div1 a b = (a - 1) `div` b

day15 :: Text -> IO (String, String)
day15 input = do
  let cave = parseInput input
      caveExit = fst . M.findMax $ cave
      cave' = extendedCave cave
      caveExit' = fst . M.findMax $ cave'
  return (show $ findRisk cave caveExit, show $ findRisk cave' caveExit')