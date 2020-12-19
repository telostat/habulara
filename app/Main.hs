{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy                       as BL
import           Data.Csv                                   (Name)
import           Data.Habulara
import           Data.Habulara.Internal.Combinators.Commons
import qualified Data.Vector                                as V
import           System.Environment                         (getArgs)
import           System.IO                                  (hPutStrLn, stderr, stdout)


main :: IO ()
main = do
  path <- head <$> getArgs
  content <- BL.readFile path
  case readRecords mapper ',' content of
    Left err -> hPutStrLn stderr $ "Error while reading the header: " <> err
    Right rs -> writeRecords stdout (V.fromList fields) rs


mapper :: [Operator]
mapper =
  [ select "id"          >> asNEText
  , select "name"        >> asText
  , select "temperature" >> asDecimal
  , select "temperature" >> rename "temperature (F) - 1" >> asDecimal >> toFahrenheit
  , use    "temperature" >> rename "temperature (F) - 2" >> toFahrenheit
  , useAs  "temperature" "temperature (F) - 3" >> toFahrenheit
  , select "precipitation" >> asDecimal >> asPercentage
  ]


fields :: [Name]
fields =
  [ "id"
  , "name"
  , "temperature"
  , "temperature (F) - 1"
  , "temperature (F) - 2"
  , "temperature (F) - 3"
  , "precipitation"
  ]


toFahrenheit :: Operator
toFahrenheit = multiply 9 >> divideBy 5 >> add 32
