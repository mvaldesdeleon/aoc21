{-# LANGUAGE TemplateHaskell #-}

module Day2
  ( day2,
  )
where

import Control.Lens
import Data.Attoparsec.Text hiding (take)
import Relude hiding (Down)

data Command a = Forward a | Down a | Up a
  deriving (Show)

data Submarine a = Submarine {_sDepth :: a, _sHPos :: a}
  deriving (Show)

makeLenses ''Submarine

applyCommand :: Num a => Command a -> Submarine a -> Submarine a
applyCommand cmd sub =
  case cmd of
    Forward a -> sub & sHPos +~ a
    Down a -> sub & sDepth +~ a
    Up a -> sub & sDepth -~ a

processCommands :: Num a => (Command a -> Submarine a -> Submarine a) -> [Command a] -> Submarine a
processCommands processor = foldr processor (Submarine 0 0)

parseCommand :: Parser a -> Parser (Command a)
parseCommand pa = parseKeyword <* space <*> pa
  where
    parseKeyword = (string "forward" $> Forward) <|> (string "down" $> Down) <|> (string "up" $> Up)

parseInput :: Text -> [Command Integer]
parseInput input =
  case parseOnly (parseCommand decimal `sepBy1` endOfLine) input of
    Right cmds -> cmds
    Left err -> error $ fromString err

day2 :: Text -> IO (String, String)
day2 input = do
  let commands = parseInput input
      part1 = processCommands applyCommand commands
  return (show $ _sDepth part1 * _sHPos part1, "N/A")