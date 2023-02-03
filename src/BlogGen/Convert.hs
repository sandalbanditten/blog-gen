module BlogGen.Convert where

import qualified BlogGen.Markup as MU
import qualified BlogGen.Html   as HTML

-- Glue code for converting a markup document into an html document

convert :: HTML.Title -> MU.Document -> HTML.Html
convert title = HTML.html_ title . foldMap convertStructure

convertStructure :: MU.Structure -> HTML.Structure
convertStructure struct =
  case struct of
    MU.Heading n txt ->
      HTML.h_ n txt
    MU.Paragraph p ->
      HTML.p_ p
    MU.UnorderedList list ->
      HTML.ul_ $ map HTML.p_ list
    MU.OrderedList list ->
      HTML.ol_ $ map HTML.p_ list
    MU.CodeBlock list ->
      HTML.code_ (unlines list)
