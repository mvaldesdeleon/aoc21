module Aoc21Main where

import Control.Applicative ((<**>))
import Control.Monad (when)
import qualified Data.ByteString as BS
import Day1 (day1)
import Day10 (day10)
import Day11 (day11)
import Day2 (day2)
import Day3 (day3)
import Day4 (day4)
import Day5 (day5)
import Day6 (day6)
import Day7 (day7)
import Day8 (day8)
import Day9 (day9)
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R
import qualified Options.Applicative as OA
import Paths_aoc21 (getDataFileName)
import Relude
import Session (session)
import System.Directory (canonicalizePath, doesFileExist)

data Day = Day1 | Day2 | Day3 | Day4 | Day5 | Day6 | Day7 | Day8 | Day9 | Day10 | Day11
  deriving (Show, Enum)

dayId :: Day -> String
dayId day = show $ toInteger (fromEnum day) + 1

inputFilePath :: Day -> IO FilePath
inputFilePath day = getDataFileName ("inputs/day-" <> dayId day <> ".txt")

loadInput :: Day -> IO Text
loadInput day = do
  filepath <- inputFilePath day
  cached <- doesFileExist filepath
  if cached
    then do
      putStrLn "Using input file from cache"
      readFileText filepath
    else do
      putStrLn "Downloading input file"
      downloadAndSave day filepath

downloadAndSave :: Day -> FilePath -> IO Text
downloadAndSave day filepath = R.runReq R.defaultHttpConfig $ do
  response <- R.req R.GET (R.https "adventofcode.com" /: "2021" /: "day" /: toText (dayId day) /: "input") R.NoReqBody R.bsResponse (R.header "cookie" ("session=" <> session))
  let status = R.responseStatusCode response
  when (status /= 200) (error $ "Recived non-200 status code: " <> show status)
  let body = BS.init $ R.responseBody response
  writeFileBS filepath body
  return $ decodeUtf8 body

dayOption :: OA.Parser Day
dayOption =
  OA.subparser $
    pureCommand "day1" "Day 1: Sonar Sweep" Day1
      <> pureCommand "day2" "Day 2: Dive!" Day2
      <> pureCommand "day3" "Day 3: Binary Diagnostic" Day3
      <> pureCommand "day4" "Day 4: Giant Squid" Day4
      <> pureCommand "day5" "Day 5: Hydrothermal Venture" Day5
      <> pureCommand "day6" "Day 6: Lanternfish" Day6
      <> pureCommand "day7" "Day 7: The Treachery of Whales" Day7
      <> pureCommand "day8" "Day 8: Seven Segment Search" Day8
      <> pureCommand "day9" "Day 9: Smoke Basin" Day9
      <> pureCommand "day10" "Day 10: Syntax Scoring" Day10
      <> pureCommand "day11" "Day 11: Dumbo Octopus" Day11

pureCommand :: String -> String -> a -> OA.Mod OA.CommandFields a
pureCommand cmd desc val = OA.command cmd (OA.info (pure val) (OA.progDesc desc))

versionOption = OA.infoOption "1.0" (OA.short 'v' <> OA.long "version" <> OA.help "Show version" <> OA.hidden)

main :: IO ()
main = do
  (showPath, inputPath, day) <- OA.execParser (OA.info options (OA.fullDesc <> OA.progDesc "Runs the challenges for the given day" <> OA.header "Advent of Code 2021"))
  putStrLn $ "Running day " <> dayId day
  let manualInput = inputPath /= ""
  inputPath' <- canonicalizePath inputPath
  when showPath $ do
    path <- inputFilePath day
    putStrLn $ "Input file path: " <> path
  when manualInput $ do
    putStrLn $ "Override input file path: " <> inputPath'
  input <-
    if manualInput
      then readFileText inputPath'
      else loadInput day
  (part1, part2) <- case day of
    Day1 -> day1 input
    Day2 -> day2 input
    Day3 -> day3 input
    Day4 -> day4 input
    Day5 -> day5 input
    Day6 -> day6 input
    Day7 -> day7 input
    Day8 -> day8 input
    Day9 -> day9 input
    Day10 -> day10 input
    Day11 -> day11 input
  putStrLn $ "Part 1: " <> part1
  putStrLn $ "Part 2: " <> part2
  where
    options = (,,) <$> pathOption <*> inputOption <*> dayOption <**> versionOption <**> OA.helper
    pathOption = OA.switch (OA.long "show-path" <> OA.help "Show the input file path")
    inputOption = OA.strOption (OA.long "input" <> OA.help "Override the input file path" <> OA.metavar "PATH" <> OA.value "")