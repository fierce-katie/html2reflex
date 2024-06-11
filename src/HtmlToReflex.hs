{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module HtmlToReflex (htmlToReflex) where

import Data.Reflection
import Data.String
import Data.Text as T
import Prelude as P
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree

htmlToReflex :: Given Int => Text -> Text
htmlToReflex = T.concat . P.map (formatTree 0) . parseTree

formatTree :: Given Int => Int -> TagTree Text -> Text
formatTree n = \case
  TagLeaf (formatTag n -> tag) | not (T.null tag) -> shift n <> tag <> "\n"
  TagBranch el attrs inner -> T.concat $
    [ shift n, T.concat $ formatElem n el attrs, " "
    , innerFunc inner, "\n", formatInner inner]
  _ -> ""
  where
    innerFunc = \case
      [] -> ""
      [_] -> "$"
      _ -> "do"
    formatInner = \case
      [] -> "blank"
      inner -> T.concat $ P.map (formatTree (n+1)) inner

formatElem :: (Eq a, Show a, IsString a, Given Int) =>
  Int -> a -> [Attribute Text] -> [Text]
formatElem n el attrs
  | el == "input" = elemWithConfig "input"
  | el == "select" = elemWithConfig "select"
  | el == "textarea" = elemWithConfig "textArea"
  | P.null attrs = ["el ", toText el]
  | el == "div" && classOnly attrs =
    ["divClass ", toText $ snd (P.head attrs)]
  | classOnly attrs = ["elClass ", toText el, " ", toText $ snd (P.head attrs)]
  | otherwise = ["elAttr ", toText el, " ", formatAttrs attrs]
  where
    elemWithConfig e =
      [ e, "Element (def\n"
      , shift (n+1), "& initialAttributes .~ ", formatAttrs attrs, "\n"
      , shift (n+1), "-- & ", e, "ElementConfig_initialValue .~ _\n"
      , shift n, ")"]

formatTag :: Given Int => Int -> Tag Text -> Text
formatTag n = \case
  TagText (T.strip -> txt) | not (T.null txt) -> T.concat
    [ "text \""
    , T.intercalate " " . P.map T.strip $  T.lines txt
    , "\"" ]
  -- <img>, <br> and alike
  TagOpen el attrs -> T.concat $ formatElem n el attrs ++ [" blank"]
  _ -> ""

formatAttrs :: [Attribute Text] -> Text
formatAttrs = parens . T.intercalate " <> " . P.map toAttr
  where
    parens txt = "(" <> txt <> ")"
    toAttr (k,v) = toText k <> " =: " <> toText v

toText :: Show a => a -> Text
toText = T.pack . show

shift :: Given Int => Int -> Text
shift = flip T.replicate " " . (*given)

classOnly :: [Attribute Text] -> Bool
classOnly [("class",_)] = True
classOnly _ = False
