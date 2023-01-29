import Html

myHtml :: Html
myHtml = html_ "This is the title"
       $ h1_ "This is the header"
       <> p_ "This is the paragraph"

main :: IO ()
main = do
  putStrLn $ render myHtml
