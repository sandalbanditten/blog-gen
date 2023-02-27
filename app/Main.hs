module Main where

import           BlogGen
import           OptParse

import           System.Directory (doesFileExist)
import           System.Exit      (exitFailure)
import           System.IO

-- * Main

main :: IO ()
main = do
  options <- parse
  case options of
    -- Convert a whole directory
    ConvertDir input output ->
      convertDirectory input output

    -- Convert a single input, defaulting to using stdin and stdout
    -- Will ask to override files and exit with failure if not
    ConvertSingle input output -> do
      (title, inputHandle) <-
        case input of
          Stdin ->
            pure ("", stdin)
          InputFile file ->
            (,) file <$> openFile file ReadMode

      outputHandle <-
        case output of
          Stdout -> pure stdout
          OutputFile file -> do
            exists <- doesFileExist file
            shouldOpenFile <-
              if exists
                then confirm
                else pure True
            if shouldOpenFile
              then
                openFile file WriteMode
              else
                exitFailure

      convertSingle title inputHandle outputHandle
      hClose inputHandle
      hClose outputHandle

test = lk $ and <* l <*> l *>

-- * Utilities

-- Confirms a user action
confirm :: IO Bool
confirm = putStr "Are you sure? (y/N) " >>
  getLine >>= \case
    "y" -> pure True
    _   -> pure False
