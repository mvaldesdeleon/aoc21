module Day12
  ( day12,
  )
where

import Data.Attoparsec.Text
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Relude

data Key = Big String | Small String
  deriving (Show, Eq, Ord)

isBig :: Key -> Bool
isBig (Big _) = True
isBig _ = False

isSmall :: Key -> Bool
isSmall (Small _) = True
isSmall _ = False

kStart, kEnd :: Key
kStart = Small "start"
kEnd = Small "end"

data Cave = Cave {_cKey :: Key, _cNext :: [Key]}
  deriving (Show)

newtype CaveSystem = CaveSystem (M.Map Key Cave)
  deriving (Show)

parseCaveSystem :: Parser CaveSystem
parseCaveSystem = do
  edges <- parseEdge `sepBy1` endOfLine
  return $ CaveSystem $ foldl' insertEdge M.empty edges
  where
    insertEdge caveMap (from, to) =
      let caveFrom = Cave from [to]
          caveTo = Cave to [from]
       in M.insertWith mergeCaves from caveFrom $ M.insertWith mergeCaves to caveTo $ caveMap
    mergeCaves a b
      | _cKey a == _cKey b = a {_cNext = _cNext a ++ _cNext b}
      | otherwise = error $ "Can't merge different caves"

parseEdge :: Parser (Key, Key)
parseEdge = (,) <$> parseKey <* char '-' <*> parseKey

parseKey :: Parser Key
parseKey = (Big <$> manyInClass ['A' .. 'Z']) <|> (Small <$> manyInClass ['a' .. 'z'])
  where
    manyInClass = many1 . satisfy . inClass

parseInput :: Text -> CaveSystem
parseInput input = case parseOnly parseCaveSystem input of
  Right cs -> cs
  Left err -> error $ toText err

countPaths :: CaveSystem -> Integer
countPaths (CaveSystem caveMap) = go [kStart]
  where
    go [] = 0
    go path@(current : rest) =
      if current == kEnd
        then 1
        else
          let cave = caveMap M.! current
              next = filter ((||) <$> isBig <*> (`notElem` path)) $ _cNext cave
           in sum (map (go . (: path)) next)

countPaths' :: CaveSystem -> Integer
countPaths' (CaveSystem caveMap) = go [kStart]
  where
    go [] = 0
    go path@(current : rest) =
      if current == kEnd
        then 1
        else
          let cave = caveMap M.! current
              next = filter (canBeVisited path) $ _cNext cave
           in sum (map (go . (: path)) next)
    canBeVisited path next = isBig next || (next /= kStart && onlyOneSmallDuplicate (next : path))
    onlyOneSmallDuplicate path =
      let smallCaves = filter isSmall path
       in length smallCaves - length (L.nub smallCaves) <= 1

day12 :: Text -> IO (String, String)
day12 input =
  let caveSystem = parseInput input
   in return (show $ countPaths caveSystem, show $ countPaths' caveSystem)
