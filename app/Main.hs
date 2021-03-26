module Main where

import qualified Data.ByteString.Lazy           as BL
import           Data.Habulara.Dsl              (runIntoHandle)
import           Data.Habulara.Inspect.Internal (inspect)
import           Data.Version                   (showVersion)
import qualified Options.Applicative            as OA
import           Paths_habulara                 (version)
import           System.Exit                    (ExitCode(..), exitWith)
import           System.IO                      (Handle, IOMode(..), hPutStrLn, openFile, stderr, stdin, stdout)
import           Text.Printf                    (printf)


-- | Main program entry point.
main :: IO ()
main = exitWith =<< (cliProgram =<< OA.execParser cliProgramParserInfo)


-- | CLI program.
cliProgram :: CliArguments -> IO ExitCode
cliProgram (CliArguments (CommandInspect ifp))               = inspect ifp
cliProgram (CliArguments (CommandProcess (sfp, mifp, mofp))) = process sfp (mkFilepath mifp) (mkFilepath mofp)


-- | Processes given CSV data with given specification.
process :: FilePath -> Maybe FilePath -> Maybe FilePath -> IO ExitCode
process sfp Nothing Nothing       = processAux (openFile sfp ReadMode) (pure stdin) (pure stdout)
process sfp (Just ifp) Nothing    = processAux (openFile sfp ReadMode) (openFile ifp ReadMode) (pure stdout)
process sfp Nothing (Just ofp)    = processAux (openFile sfp ReadMode) (pure stdin) (openFile ofp WriteMode)
process sfp (Just ifp) (Just ofp) = processAux (openFile sfp ReadMode) (openFile ifp ReadMode) (openFile ofp WriteMode)


-- | Auxiliary function to 'process'
processAux :: IO Handle -> IO Handle -> IO Handle -> IO ExitCode
processAux ms mi mo = do
  specContent <- BL.hGetContents =<< ms
  dataContent <- BL.hGetContents =<< mi
  result <- runIntoHandle specContent dataContent =<< mo
  case result of
    Left err     ->
      hPutStrLn stderr ("Error while processing records: " <> show err) >>
      pure (ExitFailure 1)
    Right (_, n) ->
      hPutStrLn stderr (printf "Successfully processed %d record%s. Exiting..." n (if n == 1 then "" else "s" :: String)) >>
      pure ExitSuccess


-- | CLI arguments parser.
parserProgramOptions :: OA.Parser CliArguments
parserProgramOptions = CliArguments <$> OA.hsubparser
  (  OA.command "inspect" (OA.info (CommandInspect <$> optsInspect) (OA.progDesc "Inspect the given file and produce a specification file"))
  <> OA.command "process" (OA.info (CommandProcess <$> optsProcess) (OA.progDesc "Process given CSV data with given specification"))
  )


-- | @inspect@ command arguments parser.
optsInspect :: OA.Parser FilePath
optsInspect = OA.strOption (OA.long "file" <> OA.metavar "FILE" <> OA.value "-" <> OA.help "CSV file (`-` for stdin, default)")


-- | @process@ command arguments parser.
optsProcess :: OA.Parser (FilePath, Maybe FilePath, Maybe FilePath)
optsProcess = (,,)
  <$> OA.strOption (OA.long "spec" <> OA.metavar "SPEC" <> OA.help "Habulara mapper specification filepath")
  <*> OA.optional (OA.strOption (OA.long "input" <> OA.metavar "INPUT" <> OA.help "Input CSV data filepath (`-` for stdin, default)"))
  <*> OA.optional (OA.strOption (OA.long "output" <> OA.metavar "OUTPUT" <> OA.help "Output CSV data filepath (`-` for stdout, default)"))


-- | Registry of commands.
data Command =
    CommandInspect FilePath
  | CommandProcess (FilePath, Maybe FilePath, Maybe FilePath)
  deriving Show


-- | Parsed command line arguments.
newtype CliArguments = CliArguments { cliArgumentsCommand :: Command } deriving Show


-- | CLI program information.
cliProgramParserInfo :: OA.ParserInfo CliArguments
cliProgramParserInfo = OA.info
  (OA.helper <*> parserVersionOption <*> parserProgramOptions)
  (OA.fullDesc <> OA.progDesc "Habulara" <> OA.header "habulara - CSV toolkit")


-- | Version option.
parserVersionOption :: OA.Parser (a -> a)
parserVersionOption = OA.infoOption (showVersion version) (OA.long "version" <> OA.help "Show version")


-------------
-- HELPERS --
-------------


-- | If the given filepath is '-', this should be considered as STDIN, hence 'Nothing'.
--
-- >>> mkFilepath Nothing
-- Nothing
-- >>> mkFilepath (Just "")
-- Nothing
-- >>> mkFilepath (Just "-")
-- Nothing
-- >>> mkFilepath (Just "/tmp/a.csv")
-- Just "/tmp/a.csv"
mkFilepath :: Maybe String -> Maybe String
mkFilepath = maybe Nothing go
  where
    go ""  = Nothing
    go "-" = Nothing
    go x   = Just x
