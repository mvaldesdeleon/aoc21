{-# LANGUAGE DeriveFunctor #-}

module Day8
  ( day8,
  )
where

import Data.Attoparsec.Text
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Relude

data Display a = Display {_dSequence :: [a], _dValue :: [a]}
  deriving (Show, Functor)

segmentChar :: Parser Char
segmentChar = satisfy $ inClass ['a' .. 'g']

parseDisplay :: Parser (Display String)
parseDisplay = Display <$> many1 segmentChar `sepBy1` char ' ' <* string " | " <*> many1 segmentChar `sepBy1` char ' '

parseInput :: Text -> [Display String]
parseInput input =
  case parseOnly (parseDisplay `sepBy1` endOfLine) input of
    Right displays -> displays
    Left err -> error $ toText err

decode :: Ord a => Display [a] -> Integer
decode display =
  let displays' = fmap sort display
      sequence = _dSequence displays'
      value = _dValue displays'
      justFind f = fromMaybe (error $ "Element not found") . L.find f
      findByLength n = justFind ((== n) . length) $ sequence
      one = findByLength 2
      four = findByLength 4
      lenSix = filter ((== 6) . length) $ sequence
      lenFive = filter ((== 5) . length) $ sequence
      codex =
        M.fromList
          [ (one, 1),
            (four, 4),
            (findByLength 3, 7),
            (findByLength 7, 8),
            (justFind (`contains` four) $ lenSix, 9),
            (justFind ((&&) <$> (not . (`contains` four)) <*> (`contains` one)) $ lenSix, 0),
            (justFind ((&&) <$> (not . (`contains` four)) <*> (not . (`contains` one))) $ lenSix, 6),
            (justFind (`contains` one) $ lenFive, 3),
            (justFind ((&&) <$> (not . (`contains` one)) <*> (`contains` (four L.\\ one))) $ lenFive, 5),
            (justFind ((&&) <$> (not . (`contains` one)) <*> (not . (`contains` (four L.\\ one)))) $ lenFive, 2)
          ]
   in fromMaybe 0 . readMaybe . concatMap (show . (codex M.!)) $ value
  where
    contains big small = all (`elem` big) small

day8 :: Text -> IO (String, String)
day8 input =
  let displays = parseInput input
      part1 = length . concatMap (filter (`elem` [2, 3, 4, 7]) . _dValue . fmap length) $ displays
      part2 = sum . fmap decode $ displays
   in return (show $ part1, show $ part2)
