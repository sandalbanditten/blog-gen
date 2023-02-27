module BlogGen.Directory
  ( convertDirectory
  , buildIndex
  )
  where

import           BlogGen.Convert    (convert, convertStructure)
import qualified BlogGen.Html       as Html
import qualified BlogGen.Markup     as MU

import           Control.Monad     (void, when)
import           Data.List         (partition)
import           Data.Traversable  (for)

import           Control.Exception (SomeException (..), catch, displayException)
import           System.Directory  (copyFile, createDirectory,
                                    doesDirectoryExist, listDirectory,
                                    removeDirectoryRecursive)
import           System.Exit       (exitFailure)
import           System.FilePath   (takeBaseName, takeExtension, takeFileName,
                                    (<.>), (</>))
import           System.IO         (hPutStrLn, stderr)

-- * Convert a directory

-- convertDirectory
-- Copies files from one directory to another, copying '.txt'-files
-- to '.html'-files
-- Writes to stderr in case of failure
-- May throw an exception
convertDirectory :: FilePath -> FilePath -> IO ()
convertDirecotry inDir outDir = do
  DirContents filesToProcess filesToCopy <- gitDirFilesAndContent inDir
  createOutDirOrExit outDir
  let
   outputHtmls = txtsToRenderedHtml filesToProcess
  copyFiles outDir filesToCopy
  writeFiles outDir outputHtmls
  putStrln "Done"

-- DirContents
-- The relevant files and content for our application
data DirContents
  = DirContents
    { dcFilesToProcess :: [(FilePath, String)]
    -- ^ file paths and their contents
    , dcFilesToCopy    :: [FilePath]
    -- ^ Other file paths, to be copied
    }

-- getDirFilesAndContent
-- Gets all content from a directory
getDirFilesAndContent :: FilePath -> IO DirContents

-- * IO list thingies

-- Apply IO on list
-- Applies (a -> IO b) on a list and records failures
applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList actions inputs = do
  for inputs $ \input -> do
    maybeResult <-
      catch
        (Right <$> action input)
        ( \(SomeException e) -> do
          pure $ Left (displayException e)
        )
    pure (input, maybeResult)

-- Filters and reports errors to stderr
filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures =
  foldMap $ \(file, contentOrErr) ->
    case contentOrErr of
      Left err -> do
        hPutStrLn stderr err
        pure []
      Right content ->
        pure [(file, content)]

-- * Build an index file
