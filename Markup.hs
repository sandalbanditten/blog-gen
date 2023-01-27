module Markup
  ( Document 
  , Structure(..) -- also export constructors for the type
  )

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

