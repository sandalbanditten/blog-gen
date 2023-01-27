module Html.Internal where

-- Types

newtype Html
  = Html String

newtype Structure
  = Structure String

type Title
  = String

-- EDSL

p_ :: String -> Structure
p_ = Structure . el "p" . escape

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concat . map ( el "li" . toString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concat . map ( el "li" . toString)

html_ :: Title -> Structure -> Html
html_ t c = Html
          $ el "html"
          $ (el "head" $ el "title" t) <> (el "body" $ toString c)

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b)
  = Structure (a <> b)

-- Render

render :: Html -> String
render (Html s) = s

-- Util

toString :: Structure -> String
toString (Structure s) = s

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

escape :: String -> String
escape = concat . map escapeChar

escapeChar :: Char -> String
escapeChar c = case c of
  '<'  -> "&lt;"
  '>'  -> "&gt;"
  '&'  -> "&amp;"
  '"'  -> "&quot;"
  '\'' -> "&#39;"
  _    -> [c]
