module Options where

import GHC.Generics
import Options.Applicative

data Options = Options
  { inputFile :: Maybe FilePath
  , outputFile :: Maybe FilePath
  , indentSpaces :: Int
  } deriving Generic

optParser :: ParserInfo Options
optParser = info (optParser' <**> helper) (fullDesc <>
  progDesc "Convert HTML to Haskell code for Reflex.Dom" <>
  header "html2reflex - converter from HTML to Reflex framework code")
  where
    optParser' = Options <$>
      optional (strOption
        (short 'i' <> long "input" <> metavar "FILEPATH" <> help "Input file")) <*>
      optional (strOption
        (short 'o' <> long "output" <> metavar "FILEPATH" <> help "Output file")) <*>
      option auto (short 'n' <> long "indent" <> metavar "INT" <>
        help "Number of spaces used for indentation" <> showDefault <> value 2)

