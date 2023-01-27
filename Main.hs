newtype Html = Html String

newtype Structure = Structure String

type Title = String

-- lib

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

-- bin

myHtml :: Html
myHtml = html_ "This is a title"
       $ append_ (h1_ "This is a header") (p_ "This is a paragraph")

main :: IO ()
main = do
  putStrLn $ render myHtml
