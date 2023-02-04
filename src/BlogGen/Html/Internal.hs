module BlogGen.Html.Internal where

-- Imports

import           Numeric.Natural

-- Types

newtype Html
  = Html String

newtype Structure
  = Structure String

newtype Content
  = Content String

type Title
  = String

-- Typeclasses

instance Semigroup Structure where
  (<>) a b =
    Structure $ getStructureStr a <> getStructureStr b

instance Monoid Structure where
  mempty = Structure ""

instance Semigroup Content where
  (<>) a b =
    Content $ getContentStr a <> getContentStr b

instance Monoid Content where
  mempty = Content ""

-- * EDSL

html_ :: Title -> Structure -> Html
html_ t c = Html
          $ el "html"
          $ el "head" (el "title" t) <> el "body" (getStructureStr c)

-- * Structure

p_ :: Content -> Structure
p_ = Structure . el "p" . escape . getContentStr

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

h_ :: Natural -> Content -> Structure
h_ n = Structure . el ("h" <> show n) . escape . getContentStr

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap ( el "li" . getStructureStr)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap ( el "li" . getStructureStr)

-- * Content

txt_ :: String -> Content
txt_ = Content . escape

link_ :: FilePath -> Content -> Content
link_ path content =
  Content $
    elAttr
      "a"
      ("href=\"" <> escape path <> "\"")
      (getContentStr content)

img_ :: FilePath -> Content
img_ path =
  Content $ "<img src=\"" <> escape path <> "\">"

b_ :: Content -> Content
b_ content =
  Content $ el "b" (getContentStr content)

i_ :: Content -> Content
i_ content =
  Content $ el "i" (getContentStr content)

-- * Render

render :: Html -> String
render (Html s) = s

-- * Util

getStructureStr :: Structure -> String
getStructureStr (Structure s) = s

getContentStr :: Content -> String
getContentStr (Content s) = s

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

elAttr :: String -> String -> String -> String
elAttr tag attrs content =
  "<" <> tag <> " " <> attrs <> ">" <> content <> "</" <> tag <> ">"

escape :: String -> String
escape = concatMap escapeChar

escapeChar :: Char -> String
escapeChar c = case c of
  '<'  -> "&lt;"
  '>'  -> "&gt;"
  '&'  -> "&amp;"
  '"'  -> "&quot;"
  '\'' -> "&#39;"
  _    -> [c]
