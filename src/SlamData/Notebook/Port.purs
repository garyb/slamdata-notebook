module SlamData.Notebook.Port where

import Text.Markdown.SlamDown.Html (FormFieldValue())
import qualified Data.StrMap as M

data Port
  = VarMap (M.StrMap FormFieldValue)
  | Closed
