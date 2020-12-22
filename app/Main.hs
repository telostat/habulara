{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Habulara.Dsl    (readHabFile, readWriteRecordsWithGeneratedMapper)
import           System.Environment   (getArgs)
import           System.Exit          (die)
import           System.IO            (hPutStrLn, stderr, stdout)


main :: IO ()
main = do
  (habpath, csvpath) <- getProgramArguments =<< getArgs
  eFs <- readHabFile <$> B.readFile habpath
  case eFs of
    Left err -> die $ "Invalid Hab file: " <> err
    Right fs -> do
      content <- BL.readFile csvpath
      result <- readWriteRecordsWithGeneratedMapper fs content stdout
      case result of
        Left err -> hPutStrLn stderr $ "Error while reading the file: " <> err
        Right _  -> pure ()


getProgramArguments :: [FilePath] -> IO (FilePath, FilePath)
getProgramArguments [habfile, csvfile] = pure (habfile, csvfile)
getProgramArguments _                  = die "Incorrect number of arguments. Exiting..."
