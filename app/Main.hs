module Main (main) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import HtmlToReflex (htmlToReflex)
import System.IO as IO
import Options
import Options.Applicative

main :: IO ()
main = do
  opts <- execParser optParser
  case opts.inputFile of
    Nothing -> runInteractive
    Just inp -> runWithFiles inp opts.outputFile

runInteractive :: IO ()
runInteractive = do
  putStrLn "Paste raw HTML and press <Ctrl-D> or press <Ctrl-C> to exit:"
  tty <- openFile "/dev/tty" ReadMode
  s <- hGetContents' tty
  hClose tty
  putStrLn $ "\n" <> Prelude.replicate 80 '='
  T.putStrLn $ "\nResult:\n\n" <> (htmlToReflex $ T.pack s)
  putStrLn $ Prelude.replicate 80 '='
  runInteractive

runWithFiles :: FilePath -> Maybe FilePath -> IO ()
runWithFiles input moutput = do
  txt <- T.readFile input
  let res = htmlToReflex txt
  maybe (T.putStrLn res) (flip T.writeFile res) moutput
