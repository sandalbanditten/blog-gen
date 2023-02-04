module BlogGen
  ( convertSingle
  , convertDirectory
  , process
  )
  where

import           BlogGen.Convert (convert)
import           BlogGen.Html    (Title, render)
import           BlogGen.Markup  (parse)

import           System.IO

confirm :: IO Bool
confirm =
  putStrLn "Are you sure? (y/N)" *>
    getLine >>= \case
      "y" -> pure True
      _   -> pure False

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action =
  cond >>= \case
    True  -> action
    False -> pure ()

convertSingle :: Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory = error "Not yet implemented"

process :: Title -> String -> String
process title = render . convert title . parse
