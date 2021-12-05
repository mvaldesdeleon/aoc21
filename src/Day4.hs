{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Day4
  ( day4,
  )
where

import Data.Attoparsec.Text
import Relude

newtype Board a = Board [[a]]
  deriving (Show, Functor)

boardMatch :: Eq a => a -> Board a -> Bool
boardMatch a (Board ass) = rowMatch ass || rowMatch (transpose ass)
  where
    rowMatch = any (all (== a))

initBoard :: Board a -> Board (a, Bool)
initBoard = fmap (,False)

boardBingo :: Board (a, Bool) -> Bool
boardBingo = boardMatch True . fmap snd

markBoard :: Eq a => a -> Board (a, Bool) -> Board (a, Bool)
markBoard a = fmap mark
  where
    mark (a', b) = (a', if a == a' then True else b)

boardScore :: Num a => Board (a, Bool) -> a
boardScore (Board ass) = sum . map fst . filter ((== False) . snd) . concat $ ass

playBingo :: (Eq a, Num a) => [a] -> [Board a] -> a
playBingo numbers boards = go numbers (map initBoard boards)
  where
    go (n : ns) boards =
      let boards' = map (markBoard n) boards
          winner = find boardBingo boards'
       in case winner of
            Just board -> n * boardScore board
            Nothing -> go ns boards'
    go [] _ = error $ "No winners"

lastWinner :: (Eq a, Num a) => [a] -> [Board a] -> a
lastWinner numbers boards = go numbers (map initBoard boards)
  where
    go (n : ns) boards =
      let boards' = map (markBoard n) boards
          winner = find boardBingo boards'
          rest = filter (not . boardBingo) boards'
       in if null rest
            then case winner of
              Just board -> n * boardScore board
              Nothing -> error $ "No boards left but nobody won"
            else go ns rest
    go [] _ = error $ "No winners"

parseInput :: Text -> ([Integer], [Board Integer])
parseInput input =
  case parseOnly ((,) <$> parseNumbers <* endOfLine <*> parseBoards) input of
    Right res -> res
    Left err -> error $ fromString err
  where
    parseNumbers = decimal `sepBy1` char ',' <* endOfLine
    parseBoards = parseBoard `sepBy1` (endOfLine <* endOfLine)
    parseBoard = Board <$> ((optional (char ' ') *> decimal) `sepBy1'` char ' ') `sepBy1` endOfLine

day4 :: Text -> IO (String, String)
day4 input = do
  let (numbers, boards) = parseInput input
      score = playBingo numbers boards
      score' = lastWinner numbers boards
  return (show score, show score')