module Markup
  ( Document
  , Structure(..) -- also export constructors for the type
  , parse
  )
  where

-- Imports

import           Data.Maybe      (maybeToList)
import           Numeric.Natural

-- Types

type Document
  = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Eq, Show)

-- Functions

parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines ctx txts =
  case txts of
    -- Done case
    [] -> maybeToList ctx

    -- H1 case
    ('*' : ' ' : ln) : rest ->
      maybe id (:) ctx (Heading 1 (trim ln) : parseLines Nothing rest)

    -- Unordered list case
    ('-' : ' ' : ln) : rest ->
      case ctx of
        Just (UnorderedList list) ->
          parseLines (Just (UnorderedList (list <> [trim ln]))) rest
        _ ->
          maybe id (:) ctx (parseLines (Just (UnorderedList [trim ln])) rest)

    -- Ordered list case
    ('#' : ' ' : ln) : rest ->
      case ctx of
        Just (OrderedList list) ->
          parseLines (Just (OrderedList (list <> [trim ln]))) rest
        _ ->
          maybe id (:) ctx (parseLines (Just (OrderedList [trim ln])) rest)

    -- Code block case
    ('>' : ' ' : ln) : rest ->
      case ctx of
        Just (CodeBlock code) ->
          parseLines (Just (CodeBlock (code <> [ln]))) rest
        _ ->
          maybe id (:) ctx (parseLines (Just (CodeBlock [ln])) rest)

    -- Paragraph case
    curLn:rest ->
      let
        ln = trim curLn
      in
        if ln == ""
          then
            maybe id (:) ctx (parseLines Nothing rest)
          else
            case ctx of
              Just (Paragraph paragraph) ->
                parseLines (Just (Paragraph (unwords [paragraph, ln]))) rest
              _ ->
                maybe id (:) ctx (parseLines (Just (Paragraph ln)) rest)

trim :: String -> String
trim = unwords . words
