module Day9
  ( day9,
  )
where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Relude

parseInput :: Text -> M.Map (Integer, Integer) Char
parseInput input =
  let rows = lines input
      height = genericLength rows
      width = genericLength . toString . L.head $ rows
   in M.fromAscList $ zip [(y, x) | y <- [0 .. height - 1], x <- [0 .. width - 1]] (filter (/= '\n') . toString $ input)

localMinimum :: M.Map (Integer, Integer) Char -> (Integer, Integer) -> Bool
localMinimum floor (y, x) =
  let val = floor M.! (y, x)
      top = fromMaybe '9' $ floor M.!? (y -1, x)
      left = fromMaybe '9' $ floor M.!? (y, x + 1)
      bottom = fromMaybe '9' $ floor M.!? (y + 1, x)
      right = fromMaybe '9' $ floor M.!? (y, x -1)
   in val < top && val < left && val < bottom && val < right

getLocalMinimum :: M.Map (Integer, Integer) Char -> [Char]
getLocalMinimum floor =
  let (top, left) = S.findMin $ M.keysSet floor
      (bottom, right) = S.findMax $ M.keysSet floor
   in mapMaybe maybeMinimum [(y, x) | y <- [top .. bottom], x <- [left .. right]]
  where
    maybeMinimum position =
      if localMinimum floor position
        then Just $ floor M.! position
        else Nothing

day9 :: Text -> IO (String, String)
day9 input = do
  let floor = parseInput input
      risk = map (+ 1) . mapMaybe (\x -> readMaybe [x]) . getLocalMinimum $ floor
  return (show $ sum risk, "N/A")
