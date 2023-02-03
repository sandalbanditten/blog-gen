module BlogGen
  ( main
  , process
  )
  where

import           BlogGen.Convert     (convert)
import           BlogGen.Html        (Title, render)
import           BlogGen.Markup      (parse)

import           System.Directory   (doesFileExist)
import           System.Environment (getArgs)

process :: Title -> String -> String
process title = render . convert title . parse

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

main :: IO ()
main = getArgs >>= \case -- get args
  -- No arguments; read from stdin and write to stdout
  [] -> do
    content <- getContents
    putStrLn (process "Empty title" content)

  -- With input and output file as arguments
  [input, output] -> do
    content <- readFile input
    exists  <- doesFileExist output
    let
      writeResult = writeFile output (process input content)
    if exists
      then whenIO confirm writeResult
      else writeResult

  -- Default to printing an error message
  _ -> putStrLn "Usage: Main [-- <input-file> <output-file>]"
