module SlamData.Notebook.Component where

import Prelude

import Data.Functor.Coproduct (Coproduct(), coproduct)
import Data.Generic (Generic, gEq, gCompare)
import Data.Graph (Graph())
import Data.Maybe (Maybe())
import Data.NaturalTransformation (Natural())
import Data.These (These())

import Halogen
-- import Halogen.Component.Private (PrivateState(), PrivateQuery())
import Halogen.HTML as H

import SlamData.Slam
import SlamData.Notebook.Cell.Component
import SlamData.Notebook.Cell.Fake

newtype CellSlot = CellSlot CellId
derive instance genericCellSlot :: Generic CellSlot
instance eqCellSlot :: Eq CellSlot where eq = gEq
instance ordCellSlot :: Ord CellSlot where compare = gCompare

-- type CellDef =
--   { component :: Component CellStateP CellQueryP Slam
--   , initialState :: CellState PrivateState
--   }

-- type NotebookState =
--   { cells :: Array
--       { id :: CellId
--       , definition :: CellDef
--       }
--   , dependencies :: Graph CellId CellId
--   , name :: These String String
--   , activeCellId :: Maybe CellId
--   }

-- data NotebookQuery a
--   = AddCell CellDef a
--   | RunActiveCell a

-- type NotebookQueryP = Coproduct NotebookQuery (ChildF CellSlot CellQueryP)
-- type NotebookStateP = InstalledState NotebookState CellStateP NotebookQuery CellQueryP Slam CellSlot

-- notebookComponent :: Component NotebookStateP NotebookQueryP Slam
-- notebookComponent = parentComponent' render eval peek
--   where
--   render :: NotebookState -> ParentHTML CellStateP NotebookQuery CellQueryP Slam CellSlot
--   render _ = H.div_ []

--   eval :: Natural NotebookQuery (ParentDSL NotebookState CellStateP NotebookQuery CellQueryP Slam CellSlot)
--   eval (AddCell def next) = do
--     -- add new root cell
--     pure next
--   eval (RunActiveCell next) = do
--     -- query RunCell on the active cell
--     pure next

--   peek :: forall a. ChildF CellSlot CellQueryP a -> ParentDSL NotebookState CellStateP NotebookQuery CellQueryP Slam CellSlot Unit
--   peek (ChildF slot q) = coproduct (peekCell slot) (const $ pure unit) q

--   peekCell :: forall a. CellSlot -> CellQuery a -> ParentDSL NotebookState CellStateP NotebookQuery CellQueryP Slam CellSlot Unit
--   peekCell slot (RunCell a) = do
--     -- once this completes we can update children? what about UpdateCell?
--     pure unit
--   peekCell slot (RefreshCell a) = do
--     -- go to top of dependency graph and refresh downwards
--     pure unit
--   peekCell slot (TrashCell a) = do
--     -- remove cell and dependant cells
--     pure unit
--   peekCell slot (CreateChildCell def a) = do
--     -- insert new child cell into graph
--     pure unit
--   peekCell slot (ShareCell a) = do
--     -- open share modal
--     pure unit
--   peekCell _ _ = pure unit
