{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy         as BL
import           Data.Habulara.Core.Conduit   (runMapperIntoHandle)
import           Data.Habulara.Core.Operation
import           System.Environment           (getArgs)
import           System.IO                    (hPutStrLn, stderr, stdout)


main :: IO ()
main = do
  [filepath] <- getArgs
  content <- BL.readFile filepath
  x <- runMapperIntoHandle ',' fops True content stdout
  either print (hPutStrLn stderr . ("Total number of records processed: " <>) . show . snd) x
  where
    fops =
      [ ("id", select "id")
      , ("name", select "name")
      , ("tempCelcius", select "temperature")
      , ("tempFahrenheit", select "temperature" >>= asNumber >>= multiply (number 9) >>= flip divide (number 5) >>= add (number 32))
      , ("precipitation", select "precipitation" >>= asNumber >>= percentage)
      ]
