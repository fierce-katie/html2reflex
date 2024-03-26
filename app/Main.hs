module Main (main) where

import Data.Text as T
import HtmlReflex (htmlToReflex)
import System.IO

main :: IO ()
main = go
  where
    go = do
      putStrLn "Paste raw HTML and press <Ctrl-D>, press <Ctrl-C> to exit:"
      tty <- openFile "/dev/tty" ReadMode
      s <- hGetContents' tty
      hClose tty
      putStrLn $ "\n" <> Prelude.replicate 80 '='
      putStrLn $ "\nResult:\n\n" <> (T.unpack . htmlToReflex . T.pack $ s)
      putStrLn $ Prelude.replicate 80 '='
      go
