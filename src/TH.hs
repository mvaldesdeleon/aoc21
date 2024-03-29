{-# LANGUAGE TemplateHaskellQuotes #-}

module TH
  ( makeDayT,
    makeDayC,
    makeTOC,
  )
where

import Language.Haskell.TH
import Relude

challenges :: [String]
challenges =
  [ "Day 1: Sonar Sweep",
    "Day 2: Dive!",
    "Day 3: Binary Diagnostic",
    "Day 4: Giant Squid",
    "Day 5: Hydrothermal Venture",
    "Day 6: Lanternfish",
    "Day 7: The Treachery of Whales",
    "Day 8: Seven Segment Search",
    "Day 9: Smoke Basin",
    "Day 10: Syntax Scoring",
    "Day 11: Dumbo Octopus",
    "Day 12: Passage Pathing",
    "Day 13: Transparent Origami",
    "Day 14: Extended Polymerization",
    "Day 15: Chiton",
    "Day 16: Packet Decoder"
  ]

makeDayT :: Q [Dec]
makeDayT = do
  name <- newName "Day"
  cons <- traverse makeDayC [1 .. length challenges]
  return [DataD [] name [] Nothing cons [DerivClause Nothing [ConT ''Show, ConT ''Enum]]]
  where
    makeDayC i = do
      name <- newName ("Day" ++ show i)
      return $ NormalC name []

makeDayC :: Q Exp
makeDayC = do
  return $ LamCaseE (map makeDayM [1 .. length challenges])
  where
    makeDayM i = Match (ConP (mkName $ "Day" ++ show i) [] []) (NormalB (VarE (mkName $ "day" ++ show i))) []

makeTOC :: Q Exp
makeTOC = do
  return $ ListE (zipWith makeTuple [1 ..] challenges)
  where
    makeTuple i desc = TupE [Just (LitE (StringL $ "day" ++ show i)), Just (LitE (StringL desc)), Just (ConE (mkName $ "Day" ++ show i))]