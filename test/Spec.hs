{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Habulara.Dsl          (As(..), FieldSpec(..), FileSpec(..), Op(..), process)
import           System.IO                  (hPutStrLn, stderr, stdout)


main :: IO ()
main = do
  content <- BL.readFile "test/examples/simple.csv"
  case process fileSpec content of
    Left err -> hPutStrLn stderr $ "Error while processing the file: " <> err
    Right oc -> BLC.hPutStr stdout oc


fileSpec :: FileSpec
fileSpec = FileSpec "Latest Weather Figures" Nothing Nothing ',' Nothing
  [ FieldSpec AsText "id" Nothing (Just "ID") (Just "ID of the record") False False []
  , FieldSpec AsText "name" Nothing (Just "Name") (Just "Name of the record") False False []
  , FieldSpec AsDecimal "tempCelcius" (Just "temperature") (Just "Temperature (C)") (Just "Temperature in Celcius") False False [OpDecimal]
  , FieldSpec AsDecimal "tempFahrenheit" (Just "temperature") (Just "Temperature (F)") (Just "Temperature in Fahrenheit") False False [OpDecimal, OpMultiply 9, OpDivideBy 5, OpAdd 32]
  , FieldSpec AsDecimal "precipitation" Nothing (Just "Precipitation (%)") (Just "Precipitation in percentage points") False False [OpDecimal, OpPercentage]
  ]
