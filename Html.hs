module Html
  ( Html
  , Title
  , Structure
  , html_
  , p_
  , h1_
  , append_
  , render
  )
  where

newtype Html = Html String

newtype Structure = Structure String

type Title = String

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

render :: Html -> String
render (Html s) = s

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) =
  Structure (a <> b)

p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . el "h1"

html_ :: Title -> Structure -> Html
html_ t (Structure c) = Html
                      $ el "html"
                      $ (el "head" $ el "title" t) <> (el "body" c)

