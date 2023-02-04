module BlogGen.Convert where

import qualified BlogGen.Html   as Html
import qualified BlogGen.Markup as MU

-- Glue code for converting a markup document into an html document

convert :: Html.Title -> MU.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure

convertStructure :: MU.Structure -> Html.Structure
convertStructure struct =
  case struct of
    MU.Heading n txt ->
      Html.h_ n $ Html.txt_ txt
    MU.Paragraph p ->
      Html.p_ $ Html.txt_ p
    MU.UnorderedList list ->
      Html.ul_ $ map (Html.p_ . Html.txt_) list
    MU.OrderedList list ->
      Html.ol_ $ map (Html.p_ . Html.txt_) list
    MU.CodeBlock list ->
      Html.code_ (unlines list)
