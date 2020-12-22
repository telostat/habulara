{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BL
import           Data.Habulara.Dsl    (As(..), FieldSpec(..), FileSpec(..), Op(..), readWriteRecordsWithGeneratedMapper)
import           System.IO            (hPutStrLn, stderr, stdout)


main :: IO ()
main = do
  content <- BL.readFile "test/examples/simple.csv"
  result <- readWriteRecordsWithGeneratedMapper fileSpec content stdout
  case result of
    Left err -> hPutStrLn stderr $ "Error while reading the file: " <> err
    Right _  -> pure ()


fileSpec :: FileSpec
fileSpec = FileSpec "Latest Weather Figures" Nothing Nothing ','
  [ FieldSpec AsText "id" Nothing (Just "ID") (Just "ID of the record") False False []
  , FieldSpec AsText "name" Nothing (Just "Name") (Just "Name of the record") False False []
  , FieldSpec AsDecimal "tempCelcius" (Just "temperature") (Just "Temperature (C)") (Just "Temperature in Celcius") False False [OpDecimal]
  , FieldSpec AsDecimal "tempFahrenheit" (Just "temperature") (Just "Temperature (F)") (Just "Temperature in Fahrenheit") False False [OpDecimal, OpMultiply 9, OpDivideBy 5, OpAdd 32]
  , FieldSpec AsDecimal "precipitation" Nothing (Just "Precipitation (%)") (Just "Precipitation in percentage points") False False [OpDecimal, OpPercentage]
  ]
