module Main (main) where

import Data.Text as T
import HtmlReflex (htmlToReflex)
import System.IO

main :: IO ()
main = do
  putStrLn "Paste raw HTML:"
  s <- getContents'
  putStrLn $ "Result:\n" <> (T.unpack . htmlToReflex . T.pack $ s)
