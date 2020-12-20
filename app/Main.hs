{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import           Control.Monad        ((>=>))
import qualified Data.ByteString.Lazy as BL
import           Data.Csv             (Name)
import           Data.Habulara
import qualified Data.Vector          as V
import           System.Environment   (getArgs)
import           System.IO            (hPutStrLn, stderr, stdout)


main :: IO ()
main = do
  path <- head <$> getArgs
  content <- BL.readFile path
  case readRecords mapper ',' content of
    Left err -> hPutStrLn stderr $ "Error while reading the header: " <> err
    Right rs -> writeRecords stdout (V.fromList fields) rs


mapper :: [FieldMapper]
mapper =
  [ select   "id"
  , select   "name"
  , selectAs "temperature (C)" "temperature"
  , selectAs "temperature (F)" "temperature" ~> vDecimal >=> toFahrenheit
  , select   "precipitation" ~> vDecimal >=> percentage
  ]


fields :: [Name]
fields =
  [ "id"
  , "name"
  , "temperature (C)"
  , "temperature (F)"
  , "precipitation"
  ]


toFahrenheit :: ValueOperator
toFahrenheit x = multiply 9 x >>= divideBy 5 >>= add 32
