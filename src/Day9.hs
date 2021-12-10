module Day9
  ( day9,
  )
where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Relude

type Position = (Integer, Integer)

pTop, pBottom, pLeft, pRight :: Position -> Position
pTop (y, x) = (y - 1, x)
pBottom (y, x) = (y + 1, x)
pLeft (y, x) = (y, x - 1)
pRight (y, x) = (y, x + 1)

parseInput :: Text -> M.Map Position Char
parseInput input =
  let rows = lines input
      height = genericLength rows
      width = genericLength . toString . L.head $ rows
   in M.fromAscList $ zip [(y, x) | y <- [0 .. height - 1], x <- [0 .. width - 1]] (filter (/= '\n') . toString $ input)

localMinimum :: M.Map Position Char -> Position -> Bool
localMinimum floor position =
  let val = floor M.! position
      top = fromMaybe '9' $ floor M.!? pTop position
      left = fromMaybe '9' $ floor M.!? pLeft position
      bottom = fromMaybe '9' $ floor M.!? pBottom position
      right = fromMaybe '9' $ floor M.!? pRight position
   in val < top && val < left && val < bottom && val < right

getLocalMinimum :: M.Map Position Char -> [(Position, Char)]
getLocalMinimum floor =
  let (top, left) = S.findMin $ M.keysSet floor
      (bottom, right) = S.findMax $ M.keysSet floor
   in mapMaybe maybeMinimum [(y, x) | y <- [top .. bottom], x <- [left .. right]]
  where
    maybeMinimum position =
      if localMinimum floor position
        then Just $ (position, floor M.! position)
        else Nothing

basinSize :: M.Map Position Char -> Position -> Integer
basinSize floor position = go S.empty [position]
  where
    go visited (next : rest) =
      if next `S.member` visited
        then go visited rest
        else
          let value = fromMaybe '9' $ floor M.!? next
           in if value == '9'
                then go visited rest
                else go (next `S.insert` visited) (pTop next : pLeft next : pBottom next : pRight next : rest)
    go visited [] = toInteger . S.size $ visited

day9 :: Text -> IO (String, String)
day9 input =
  let floor = parseInput input
      lowPoints = getLocalMinimum $ floor
      risk = map (+ 1) . mapMaybe (\(_, x) -> readMaybe [x]) $ lowPoints
      basins = sortOn negate . map (basinSize floor . fst) $ lowPoints
   in return (show $ sum risk, show $ product . take 3 $ basins)
