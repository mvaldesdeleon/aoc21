module Day10
  ( day10,
  )
where

import qualified Data.List as L
import Relude

data ParseResult = Corrupt Char | Incomplete String | Valid
  deriving (Show)

parseLine :: String -> ParseResult
parseLine = go []
  where
    go [] [] = Valid
    go stack [] = Incomplete $ close stack
    go stack (char : rest) =
      if char `L.elem` ("([{<" :: String)
        then go (char : stack) rest
        else case match stack char of
          Just stack' -> go stack' rest
          Nothing -> Corrupt char

close :: String -> String
close =
  map
    ( \case
        '(' -> ')'
        '[' -> ']'
        '{' -> '}'
        '<' -> '>'
        _ -> error $ "Unexpected character"
    )

match :: String -> Char -> Maybe String
match [] _ = Nothing
match (char : rest) char' =
  case (char, char') of
    ('(', ')') -> Just rest
    ('[', ']') -> Just rest
    ('{', '}') -> Just rest
    ('<', '>') -> Just rest
    _ -> Nothing

score :: ParseResult -> Integer
score Valid = 0
score (Incomplete _) = 0
score (Corrupt char) =
  case char of
    ')' -> 3
    ']' -> 57
    '}' -> 1197
    '>' -> 25137
    _ -> error $ "Unexpected character"

score' :: ParseResult -> Integer
score' Valid = 0
score' (Corrupt _) = 0
score' (Incomplete rest) = go 0 rest
  where
    go score [] = score
    go score (char : rest) =
      go
        ( score * 5
            + ( case char of
                  ')' -> 1
                  ']' -> 2
                  '}' -> 3
                  '>' -> 4
                  _ -> error $ "Unexpected character"
              )
        )
        rest

parseInput :: Text -> [String]
parseInput = map toString . words

day10 :: Text -> IO (String, String)
day10 input =
  let lines = parseInput input
      results = map parseLine lines
      autocomplete = sort . filter (/= 0) . map score' $ results
   in return (show $ sum . map score $ results, show $ autocomplete L.!! ((length autocomplete - 1) `div` 2))
