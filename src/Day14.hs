module Day14
  ( day14,
  )
where

import Data.Attoparsec.Text
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Relude

type Pair = (Char, Char)

mkPairs :: [Char] -> [Pair]
mkPairs cs = zip cs (drop 1 cs)

type Composition = M.Map Pair Integer

addToMap :: (Ord k, Num v) => k -> v -> M.Map k v -> M.Map k v
addToMap = M.insertWith (+)

data Polymer = Polymer
  { _pFirst :: Char,
    _pComposition :: Composition
  }
  deriving (Show)

parseTemplate :: Parser Polymer
parseTemplate = do
  cs <- manyTill letter endOfLine
  return
    $ Polymer
      { _pFirst = L.head cs,
        _pComposition = foldl' initialize M.empty (mkPairs cs)
      }
  where
    initialize composition pair = addToMap pair 1 composition

type Rule = (Pair, (Pair, Pair))

type Rules = M.Map Pair (Pair, Pair)

parseRule :: Parser Rule
parseRule = do
  a <- letter
  b <- letter
  string " -> "
  i <- letter
  return ((a, b), ((a, i), (i, b)))

parseRules :: Parser Rules
parseRules = M.fromList <$> parseRule `sepBy1` endOfLine

parseInput :: Text -> (Polymer, Rules)
parseInput input =
  case parseOnly ((,) <$> parseTemplate <* endOfLine <*> parseRules) input of
    Right res -> res
    Left err -> error $ toText err

polymerize :: Rules -> Polymer -> Polymer
polymerize rules start = start {_pComposition = M.foldlWithKey polymerizeStep M.empty (_pComposition start)}
  where
    polymerizeStep composition pair count =
      case M.lookup pair rules of
        Just (pairA, pairB) -> addToMap pairA count . addToMap pairB count $ composition
        Nothing -> addToMap pair count composition

result :: Polymer -> Integer
result polymer =
  let elementCount = M.foldlWithKey countPair (M.insert (_pFirst polymer) 1 M.empty) (_pComposition polymer)
      counts = snd <$> M.toList elementCount
   in L.maximum counts - L.minimum counts
  where
    countPair elementCount pair count = addToMap (snd pair) count elementCount

day14 :: Text -> IO (String, String)
day14 input = do
  let (polymer, rules) = parseInput input
      sequence = iterate (polymerize rules) polymer
  return (show $ result (sequence L.!! 10), show $ result (sequence L.!! 40))
