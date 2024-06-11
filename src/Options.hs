module Options where

import GHC.Generics
import Options.Applicative

data Options = Options
  { inputFile :: Maybe FilePath
  , outputFile :: Maybe FilePath
  } deriving Generic

optParser :: ParserInfo Options
optParser = info (optParser' <**> helper) (fullDesc <>
  progDesc "Convert HTML to Haskell code for Reflex.Dom" <>
  header "html2reflex - converter from HTML to Reflex framework code")
  where
    optParser' = Options <$>
      optional (strOption
        (short 'i' <> long "input" <> metavar "FILENAME" <> help "Input file")) <*>
      optional (strOption
        (short 'o' <> long "output" <> metavar "FILENAME" <> help "Output file"))

