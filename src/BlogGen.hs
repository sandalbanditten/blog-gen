module BlogGen
  ( convertSingle
  , convertDirectory
  , process
  )
  where

import           BlogGen.Convert (convert, convertStructure)
import           BlogGen.Html    (Html, Title, render, h_, html_, p_, txt_, link_)
import           BlogGen.Markup  (Document, Structure(..), parse)

import           System.IO

convertSingle :: Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory = error "Not yet implemented"

process :: Title -> String -> String
process title = render . convert title . parse

-- Build an index file
buildIndex :: [(FilePath, Document)] -> Html
buildIndex files =
  let
    previews =
      map
        ( \(file, doc) ->
          case doc of
            Heading 1 heading : article ->
              h_ 3 (link_ file (txt_ heading))
                <> foldMap convertStructure (take 3 article)
                <> p_ (link_ file (txt_ "..."))
            _ ->
              h_ 3 (link_ file (txt_ file))
        )
        files
  in
    html_
      "Blog"
      ( h_ 1 (link_ "index.html" (txt_ "Blog"))
        <> h_ 2 (txt_ "Posts")
        <> mconcat previews
      )
