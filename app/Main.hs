module Main where

import qualified BlogGen
import           OptParse

import           System.Directory (doesFileExist)
import           System.Exit      (exitFailure)
import           System.IO

-- * Main

main :: IO ()
main = do
  options <- parse
  case options of
    ConvertDir input output ->
      BlogGen.convertDirectory input output

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

      BlogGen.convertSingle title inputHandle outputHandle
      hClose inputHandle
      hClose outputHandle

-- * Utilities

-- Confirms a user action
confirm :: IO Bool
confirm =
  putStrLn "Are you sure? (y/N)" *>
    getLine >>= \case
      "y" -> pure True
      _   -> pure False
