module Aoc21Main where

import Control.Applicative ((<**>))
import Control.Monad (when)
import qualified Data.ByteString as BS
import Day1 (day1)
import Day2 (day2)
import Day3 (day3)
import Day4 (day4)
import Day5 (day5)
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R
import qualified Options.Applicative as OA
import Paths_aoc21 (getDataFileName)
import Relude
import Session (session)
import System.Directory (doesFileExist)

data Day = Day1 | Day2 | Day3 | Day4 | Day5
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

pureCommand :: String -> String -> a -> OA.Mod OA.CommandFields a
pureCommand cmd desc val = OA.command cmd (OA.info (pure val) (OA.progDesc desc))

versionOption = OA.infoOption "1.0" (OA.short 'v' <> OA.long "version" <> OA.help "Show version" <> OA.hidden)

main :: IO ()
main = do
  day <- OA.execParser (OA.info options (OA.fullDesc <> OA.progDesc "Runs the challenges for the given day" <> OA.header "Advent of Code 2021"))
  putStrLn $ "Running day " <> dayId day
  input <- loadInput day
  (part1, part2) <- case day of
    Day1 -> day1 input
    Day2 -> day2 input
    Day3 -> day3 input
    Day4 -> day4 input
    Day5 -> day5 input
  putStrLn $ "Part 1: " <> part1
  putStrLn $ "Part 2: " <> part2
  where
    options = dayOption <**> versionOption <**> OA.helper