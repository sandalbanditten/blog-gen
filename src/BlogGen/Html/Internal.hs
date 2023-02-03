module BlogGen.Html.Internal where

-- Imports

import           Numeric.Natural

-- Types

newtype Html
  = Html String

newtype Structure
  = Structure String

type Title
  = String

-- Typeclasses

instance Semigroup Structure where
  (<>) a b =
    Structure $ toString a <> toString b

instance Monoid Structure where
  mempty = Structure ""

-- EDSL

p_ :: String -> Structure
p_ = Structure . el "p" . escape

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

h_ :: Natural -> String -> Structure
h_ n = Structure . el ("h" <> show n) . escape

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap ( el "li" . toString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap ( el "li" . toString)

html_ :: Title -> Structure -> Html
html_ t c = Html
          $ el "html"
          $ el "head" (el "title" t) <> el "body" (toString c)

-- Render

render :: Html -> String
render (Html s) = s

-- Util

toString :: Structure -> String
toString (Structure s) = s

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

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
