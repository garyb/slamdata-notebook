module SlamData.Notebook.Cell.Fake where

-- (fakeComponent) where

-- import Prelude

-- import Data.NaturalTransformation (Natural())
-- import Data.Functor.Coproduct (Coproduct())

-- import Halogen
-- import Halogen.Component.Private (PrivateState(), PrivateQuery())
-- import Halogen.HTML as H

-- import SlamData.Notebook.Cell.Component
-- import SlamData.Notebook.Port
-- import SlamData.Slam

-- data FakeQuery a = NoOp a
-- type FakeQueryP = Coproduct CellQuery FakeQuery
-- type FakeState = {}

-- type FakeDSL = ComponentDSL (CellState FakeState) FakeQueryP Slam

-- fakeDef :: Def FakeState FakeQuery
-- fakeDef = { eval: eval, run: run, renderEditor: renderEditor, renderResults: renderResults }
--   where

--   eval :: Natural FakeQuery FakeDSL
--   eval (NoOp next) = pure next

--   run :: Port -> FakeDSL Port
--   run = pure

--   renderEditor :: FakeState -> ComponentHTML FakeQueryP
--   renderEditor _ = H.div_ []

--   renderResults :: FakeState -> ComponentHTML FakeQueryP
--   renderResults _ = H.div_ []

-- fakeComponent :: forall p. p -> SlotConstructor (CellState PrivateState) (Coproduct CellQuery PrivateQuery) Slam p
-- fakeComponent = cellComponent fakeDef {}
