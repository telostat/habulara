{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import           Data.Csv             (Name)
import           Data.Habulara
                 ( FieldParser
                 , RecordValue
                 , add
                 , compose
                 , decimal
                 , divide
                 , multiply
                 , percentagePoints
                 , readRecords
                 , rename
                 , select
                 , writeRecords
                 )
import qualified Data.Vector          as V
import           System.Environment   (getArgs)
import           System.IO            (hPutStrLn, stderr, stdout)


main :: IO ()
main = do
  path <- head <$> getArgs
  content <- BL.readFile path
  case readRecords parser ',' content of
    Left err -> hPutStrLn stderr $ "Error while reading the header: " <> err
    Right rs -> writeRecords stdout (V.fromList fields) rs


parser :: [FieldParser RecordValue]
parser =
  [ select "id"
  , select "name"
  , select "temperature" `compose` decimal `compose` rename "temperature (Celsius)"
  , select "temperature" `compose` toFahrenheit `compose` rename "temperature (Fahrenheit)"
  , select "precipitation" `compose` decimal
  , select "precipitation" `compose` percentagePoints `compose` rename "precipitation (in % points)"
  ]


fields :: [Name]
fields = ["id", "name", "temperature (Celsius)", "temperature (Fahrenheit)", "precipitation", "precipitation (in % points)"]


toFahrenheit :: FieldParser RecordValue
toFahrenheit = multiply 9 `compose` divide 5 `compose` add 32
