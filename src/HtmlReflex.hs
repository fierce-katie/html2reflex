{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module HtmlReflex (htmlToReflex) where

import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree

htmlToReflex :: Text -> Text
htmlToReflex = T.concat . map (formatTree 0) . parseTree

formatTree :: Int -> TagTree Text -> Text
formatTree n = \case
  TagLeaf (formatTag -> tag) | not (T.null tag) -> shift n <> tag <> "\n"
  TagBranch el attrs inner -> T.concat $
    [ shift n, T.concat $ formatElem el attrs, " "
    , innerFunc inner, "\n", formatInner inner]
  _ -> ""
  where
    innerFunc inner
      | null inner = ""
      | length inner == 1 = "$"
      | otherwise = "do"
    formatInner inner
      | null inner = "blank"
      | otherwise = T.concat $ map (formatTree (n+1)) inner

formatElem :: (Eq a, Show a, IsString a) => a -> [Attribute Text] -> [Text]
formatElem el attrs
  -- | el == "select"
  -- | el == "input"
  -- | el == "textarea"
  | null attrs = ["el ", toText el]
  | el == "div" && classOnly attrs =
    ["divClass ", toText $ snd (head attrs)]
  | classOnly attrs = ["elClass ", toText el, " ", toText $ snd (head attrs)]
  | otherwise = ["elAttr ", toText el, " (",
    T.intercalate " <> " (map toAttr $ attrs) <> ")"]

toText :: Show a => a -> Text
toText = T.pack . show

formatTag :: Tag Text -> Text
formatTag = \case
  TagText (T.strip -> txt) | not (T.null txt) -> "text \"" <> txt <> "\""
  TagOpen el attrs -> T.concat $ formatElem el attrs
  _ -> ""

shift :: Int -> Text
shift = flip T.replicate " " . (*2)

classOnly :: [Attribute Text] -> Bool
classOnly [("class",_)] = True
classOnly _ = False

toAttr :: Attribute Text -> Text
toAttr (k,v) = toText k <> " =: " <> toText v
