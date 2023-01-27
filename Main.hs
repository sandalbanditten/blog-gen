import Html

myHtml :: Html
myHtml = html_ "This is a title"
       $ h1_ "This is a header"
       <> p_ "This is a paragraph"

main :: IO ()
main = do
  putStrLn $ render myHtml
