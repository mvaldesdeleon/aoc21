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

unPairs :: [Pair] -> [Char]
unPairs ps = fst (L.head ps) : map snd ps

type Rules = M.Map Pair (Pair, Pair)

parseTemplate :: Parser [Pair]
parseTemplate = do
  cs <- manyTill letter endOfLine
  return $ mkPairs cs

parseRule :: Parser (Pair, (Pair, Pair))
parseRule = do
  a <- letter
  b <- letter
  string " -> "
  i <- letter
  return ((a, b), ((a, i), (i, b)))

parseRules :: Parser Rules
parseRules = M.fromList <$> parseRule `sepBy1` endOfLine

parseInput :: Text -> ([Pair], Rules)
parseInput input =
  case parseOnly ((,) <$> parseTemplate <* endOfLine <*> parseRules) input of
    Right res -> res
    Left err -> error $ toText err

polymerize :: Rules -> [Pair] -> [Pair]
polymerize rules = foldMap (insert rules)
  where
    insert rules pair =
      case M.lookup pair rules of
        Just (pa, pb) -> [pa, pb]
        Nothing -> [pair]

part1 :: String -> Integer
part1 polymer =
  let counts = sort . map length . group . sort $ polymer
   in toInteger (L.last counts - L.head counts)

day14 :: Text -> IO (String, String)
day14 input = do
  let (template, rules) = parseInput input
      polymers = iterate (polymerize rules) template
      step10 = unPairs (polymers L.!! 10)
      step40 = unPairs (polymers L.!! 40)
  return (show $ part1 step10, show $ part1 step40)
