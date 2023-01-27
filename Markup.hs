module Markup
  ( Document 
  , Structure(..) -- also export constructors for the type
  )
  where

-- Imports

import Numeric.Natural

-- Types

type Document
  = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving Show

-- Functions

parse :: String -> Document
parse = parseLines [] . lines

parseLines :: [String] -> [String] -> Document
peraselines curPar txts =
  let
    paragraph = Paragraph $ unlines $ reverse curPar
  in
    case txts of
      [] -> [paragraph]
      curLine:rest ->
        if trim curLine == ""
          then
            paragraph:parseLines [] rest
          else
            parseLines (curLine:curPar) rest

trim :: String -> String
trim = unwords . words
