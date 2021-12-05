{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Day5
  ( day5,
  )
where

import Control.Lens
import Data.Attoparsec.Text
import Data.List ((\\))
import qualified Data.List as L
import Relude

data Coord a = Coord {_cX :: a, _cY :: a}
  deriving (Show, Eq)

data Vent a = Vent {_vFrom :: Coord a, _vTo :: Coord a}
  deriving (Show, Eq)

data EventType = VentStart | VentEnd
  deriving (Show, Eq, Ord)

data Event a = Event {_eCoord :: Coord a, _eVent :: Vent a, _eType :: EventType}
  deriving (Show)

makeLenses ''Coord
makeLenses ''Vent
makeLenses ''Event

ventVertical :: Eq a => Vent a -> Bool
ventVertical = (==) <$> view (vFrom . cX) <*> view (vTo . cX)

ventHorizontal :: Eq a => Vent a -> Bool
ventHorizontal = (==) <$> view (vFrom . cY) <*> view (vTo . cY)

horizontalEnd :: Eq a => Event a -> Bool
horizontalEnd Event {_eVent, _eType} = ventHorizontal _eVent && _eType == VentEnd

horizontalStart :: Eq a => Event a -> Bool
horizontalStart Event {_eVent, _eType} = ventHorizontal _eVent && _eType == VentStart

verticalStart :: Eq a => Event a -> Bool
verticalStart Event {_eVent, _eType} = ventVertical _eVent && _eType == VentStart

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

makeEvents :: Ord a => Vent a -> [Event a]
makeEvents vent
  | ventHorizontal vent = makeEvent' cX vent
  | ventVertical vent = makeEvent' cY vent
  | otherwise = error $ "Vent is neither horizontal nor vertical"
  where
    makeEvent' cLens vent =
      let from = vent ^. vFrom
          to = vent ^. vTo
       in if from ^. cLens <= to ^. cLens
            then [Event from vent VentStart, Event to vent VentEnd]
            else [Event to vent VentStart, Event from vent VentEnd]

overlaps :: Num a => Getting a (Coord a) a -> [Event a] -> a
overlaps cLens = go 0 0 0
  where
    go last overlap result (e : es) =
      let current = e ^. eCoord . cLens
          result' = result + fromIntegral (if overlap > 1 then 1 else 0) * (current - last + 1)
          overlap' = if e ^. eType == VentStart then overlap + 1 else overlap - 1
       in go current overlap' result' es
    go _ _ result [] = result

intersections :: Ord a => [[Event a]] -> Integer
intersections = go [] 0
  where
    go current result (es : ess) =
      let current' = current ++ (map (view eVent) . filter horizontalStart $ es)
          result' = result + intersections' current' (map (view eVent) . filter verticalStart $ es)
          current'' = current' \\ (map (view eVent) . filter horizontalEnd $ es)
       in go current'' result' ess
    go _ result [] = result

    intersections' :: Ord a => [Vent a] -> [Vent a] -> Integer
    intersections' hVents vVents = sum . map (toNumber . (\hVent -> any (intersections'' hVent) vVents)) $ hVents

    intersections'' :: Ord a => Vent a -> Vent a -> Bool
    intersections'' hVent vVent = inRange (hVent ^. vFrom . cX) (hVent ^. vTo . cX) (vVent ^. vFrom . cX) && inRange (vVent ^. vFrom . cY) (vVent ^. vTo . cY) (hVent ^. vFrom . cY)

    inRange :: Ord a => a -> a -> a -> Bool
    inRange a b x = (a <= x && x <= b) || (a >= x && x >= b)

    toNumber True = 1
    toNumber False = 0

day5 :: Text -> IO (String, String)
day5 input = do
  let vents = parseInput input
      hEvents = concatMap makeEvents . filter ventHorizontal $ vents
      vEvents = concatMap makeEvents . filter ventVertical $ vents
      hOverlaps = sum . map (overlaps cX) . L.groupBy ((==) `on` view (eCoord . cY)) . L.sortBy (comparing (view (eCoord . cY)) <> comparing (view (eCoord . cX)) <> comparing (view eType)) $ hEvents
      vOverlaps = sum . map (overlaps cY) . L.groupBy ((==) `on` view (eCoord . cX)) . L.sortBy (comparing (view (eCoord . cX)) <> comparing (view (eCoord . cY)) <> comparing (view eType)) $ vEvents
      intersects = intersections . L.groupBy ((==) `on` view (eCoord . cX)) . L.sortBy (comparing (view (eCoord . cX)) <> comparing (view (eCoord . cY))) $ hEvents ++ vEvents

  return (show $ hOverlaps + vOverlaps + intersects, "N/A")
