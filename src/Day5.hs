{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Day5
  ( day5,
  )
where

import Control.Lens
import Data.Attoparsec.Text
import qualified Data.Map.Strict as M
import Relude

data Coord a = Coord {_cX :: a, _cY :: a}
  deriving (Show, Eq, Ord)

data Vent a = Vent {_vFrom :: Coord a, _vTo :: Coord a}
  deriving (Show, Eq)

makeLenses ''Coord
makeLenses ''Vent

ventVertical :: Eq a => Vent a -> Bool
ventVertical = (==) <$> view (vFrom . cX) <*> view (vTo . cX)

ventHorizontal :: Eq a => Vent a -> Bool
ventHorizontal = (==) <$> view (vFrom . cY) <*> view (vTo . cY)

parseCoord :: Parser a -> Parser (Coord a)
parseCoord pa = Coord <$> pa <* char ',' <*> pa

parseVent :: Parser a -> Parser (Vent a)
parseVent pa = Vent <$> parseCoord' <* string " -> " <*> parseCoord'
  where
    parseCoord' = parseCoord pa

parseInput :: Text -> [Vent Integer]
parseInput input = case parseOnly (parseVent decimal `sepBy1` endOfLine) input of
  Right vents -> vents
  Left err -> error $ fromString err

trackVent :: (Ord a, Enum a) => M.Map (Coord a) Integer -> Vent a -> M.Map (Coord a) Integer
trackVent floor vent = foldl' updateCoord floor (ventCoords vent)
  where
    updateCoord floor coord = M.insertWith (+) coord 1 floor

ventCoords :: (Ord a, Enum a) => Vent a -> [Coord a]
ventCoords vent
  | ventHorizontal vent =
    let fromX = vent ^. vFrom . cX
        toX = vent ^. vTo . cX
        fromY = vent ^. vFrom . cY
     in [Coord x fromY | x <- range fromX toX]
  | ventVertical vent =
    let fromY = vent ^. vFrom . cY
        toY = vent ^. vTo . cY
        fromX = vent ^. vFrom . cX
     in [Coord fromX y | y <- range fromY toY]
  | otherwise =
    let fromY = vent ^. vFrom . cY
        toY = vent ^. vTo . cY
        fromX = vent ^. vFrom . cX
        toX = vent ^. vTo . cX
     in zipWith Coord (range fromX toX) (range fromY toY)

range :: (Ord a, Enum a) => a -> a -> [a]
range a b = if a <= b then [a, succ a .. b] else [a, pred a .. b]

day5 :: Text -> IO (String, String)
day5 input =
  let vents = parseInput input
      axisAligned = filter ((||) <$> ventVertical <*> ventHorizontal) vents
      floor = foldl' trackVent M.empty axisAligned
      overlapping = M.filter (> 1) floor
      floor' = foldl' trackVent M.empty vents
      overlapping' = M.filter (> 1) floor'
   in return (show $ M.size overlapping, show $ M.size overlapping')
