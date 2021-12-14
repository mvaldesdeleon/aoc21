module Day13
  ( day13,
  )
where

import Data.Attoparsec.Text
import qualified Data.Set as S
import Relude

type Position = (Integer, Integer)

type Paper = S.Set Position

data Fold = FoldUp Integer | FoldLeft Integer
  deriving (Show)

data ManualPage = ManualPage {_mpPaper :: Paper, _mpInstructions :: [Fold]}
  deriving (Show)

parsePaper :: Parser Paper
parsePaper = S.fromList <$> parsePosition `sepBy1` endOfLine
  where
    parsePosition :: Parser Position
    parsePosition = (,) <$> decimal <* char ',' <*> decimal

parseInstructions :: Parser [Fold]
parseInstructions = parseFold `sepBy1` endOfLine
  where
    parseFold = string "fold along " *> (char 'y' $> FoldUp <|> char 'x' $> FoldLeft) <* char '=' <*> decimal

parseManualPage :: Parser ManualPage
parseManualPage = ManualPage <$> parsePaper <* count 2 endOfLine <*> parseInstructions

parseInput :: Text -> ManualPage
parseInput input =
  case parseOnly parseManualPage input of
    Right mp -> mp
    Left err -> error $ toText err

foldPaper :: Paper -> Fold -> Paper
foldPaper p (FoldUp fy) = S.map (\(x, y) -> if y > fy then (x, 2 * fy - y) else (x, y)) p
foldPaper p (FoldLeft fx) = S.map (\(x, y) -> if x > fx then (2 * fx - x, y) else (x, y)) p

foldOnce :: ManualPage -> ManualPage
foldOnce mp =
  let paper = _mpPaper mp
      (fold : rest) = _mpInstructions mp
   in ManualPage (foldPaper paper fold) rest

foldAll :: ManualPage -> ManualPage
foldAll mp =
  let paper = _mpPaper mp
      instructions = _mpInstructions mp
   in ManualPage (foldl' foldPaper paper instructions) []

size :: ManualPage -> Integer
size = toInteger . S.size . _mpPaper

printPage :: ManualPage -> String
printPage mp = concatMap printRow [0 .. height]
  where
    paper = _mpPaper mp
    width = S.findMax $ S.map fst paper
    height = S.findMax $ S.map snd paper
    printRow y = map (\x -> if (x, y) `S.member` paper then '#' else ' ') [0 .. width] ++ "\n"

day13 :: Text -> IO (String, String)
day13 input = do
  let manualPage = parseInput input
  return (show $ size $ foldOnce manualPage, "\n" <> printPage (foldAll manualPage))
