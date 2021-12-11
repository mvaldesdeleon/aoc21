{-# LANGUAGE TemplateHaskell #-}

module TH
  ( makeDayT,
    makeDayC,
  )
where

import Language.Haskell.TH
import Relude

makeDayT :: Int -> Q [Dec]
makeDayT count = do
  name <- newName "Day"
  cons <- traverse makeDayC [1 .. count]
  return [DataD [] name [] Nothing cons [DerivClause Nothing [ConT ''Show, ConT ''Enum]]]
  where
    makeDayC i = do
      name <- newName ("Day" ++ show i)
      return $ NormalC name []

makeDayC :: Int -> Q Exp
makeDayC count = do
  return $ LamCaseE (map makeDayM [1 .. count])
  where
    makeDayM i = Match (ConP (mkName $ "Day" ++ show i) []) (NormalB (VarE (mkName $ "day" ++ show i))) []