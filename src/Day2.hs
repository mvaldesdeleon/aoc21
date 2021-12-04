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

data Submarine a = Submarine {_sDepth :: a, _sHPos :: a, _sAim :: a}
  deriving (Show)

makeLenses ''Submarine

applyCommand :: Num a => Submarine a -> Command a -> Submarine a
applyCommand sub cmd =
  case cmd of
    Forward a -> sub & sHPos +~ a
    Down a -> sub & sDepth +~ a
    Up a -> sub & sDepth -~ a

applyCommand' :: Num a => Submarine a -> Command a -> Submarine a
applyCommand' sub cmd =
  case cmd of
    Forward a -> (sub & sHPos +~ a) & sDepth +~ (a * _sAim sub)
    Down a -> sub & sAim +~ a
    Up a -> sub & sAim -~ a

processCommands :: Num a => (Submarine a -> Command a -> Submarine a) -> [Command a] -> Submarine a
processCommands processor = foldl' processor (Submarine 0 0 0)

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
day2 input =
  let commands = parseInput input
      part1 = processCommands applyCommand commands
      part2 = processCommands applyCommand' commands
   in return (show $ _sDepth part1 * _sHPos part1, show $ _sDepth part2 * _sHPos part2)