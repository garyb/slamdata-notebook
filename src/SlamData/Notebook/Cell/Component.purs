module SlamData.Notebook.Cell.Component where

import Prelude

import Control.Plus (empty)
import Control.Monad.Free (Free(), mapF, liftF)

import Data.Const (Const(..))
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct(), coproduct, left, right)
import Data.Generic (Generic, gEq, gCompare)
import Data.Maybe (Maybe(..), maybe)
import Data.NaturalTransformation (Natural())

import Unsafe.Coerce (unsafeCoerce)

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.Query.StateF (mapState)
import Halogen.Query.SubscribeF (remapSubscribe)

import SlamData.Notebook.Cell.Query
import SlamData.Notebook.Cell.State
import SlamData.Notebook.Port
import SlamData.Slam

import Photons.Fold ((^?), preview)
import Photons.Prism (APrismP(), PrismP(), review, clonePrism)

-- | The slot address value for cells and identifier within the notebook graph.
newtype CellId = CellId Int

derive instance genericCellId :: Generic CellId
instance eqCellId :: Eq CellId where eq = gEq
instance ordCellId :: Ord CellId where compare = gCompare

-- | The common query algebra for a notebook cell.
data CellQuery a
  = RunCell a
  | UpdateCell Port a
  | RefreshCell a
  | TrashCell a
  | CreateChildCell Unit a
  | ToggleCellVisibility Boolean a
  | ShareCell a

-- | The common state value for a notebook cell.
type CellState s =
  { editorVisible :: Boolean
  , innerState :: s
  , output :: Port
  }

-- | Creates a `CellState` value for a given inner state value.
initCellState :: forall s. s -> CellState s
initCellState is =
  { editorVisible: true
  , innerState: is
  , output: Closed
  }

-- | A cell component definition.
-- |
-- | The `run` function is called when an `UpdateCell` query is received by the
-- | component constructed for this definition.
type Def s f =
  { renderEditor :: s -> ComponentHTML (Coproduct CellQuery f)
  , renderResults :: s -> ComponentHTML (Coproduct CellQuery f)
  , eval :: Natural f (ComponentDSL (CellState s) (Coproduct CellQuery f) Slam)
  , run :: Port -> ComponentDSL (CellState s) (Coproduct CellQuery f) Slam Port
  , _State :: APrismP AnyCellState s
  , _Query :: forall a. APrismP (AnyCellQuery a) (f a)
  }

type CellStateP = CellState AnyCellState
type CellQueryP = Coproduct CellQuery AnyCellQuery

mkCellComponent
  :: forall s f
   . Def s f
  -> Component CellStateP CellQueryP Slam
mkCellComponent def = component render (coproduct eval evalInner)
  where

  render :: CellStateP -> ComponentHTML CellQueryP
  render cs = case cs.innerState ^? _State of
    Nothing -> H.div_ []
    Just innerState ->
      H.div_
        [ H.div_
            [ H.div_ [ H.text "Cell icon" ]
            , H.div_ [ H.text "Cell type title" ]
            , H.div_ [ H.text "Cell icons" ]
            ]
        , H.div_
            [ translateF <$> def.renderEditor innerState ]
        , H.div_
            [ H.div_ [ H.text "Play button" ]
            , H.div_ [ H.text "Status summary" ]
            , H.div_ [ H.text "Result icons" ]
            ]
        , H.div_
            [ translateF <$> def.renderResults innerState ]
        ]

  eval :: Natural CellQuery (ComponentDSL CellStateP CellQueryP Slam)
  eval (RunCell next) = pure next
  eval (UpdateCell input next) = do
    cs <- get
    case fromState' cs of
      Nothing -> halt
      Just cs' -> do
        output <- mapF (eta cs') $ def.run input
        modify (_ { output = output })
        pure next
  eval (RefreshCell next) = pure next
  eval (TrashCell next) = pure next
  eval (CreateChildCell _ next) = pure next
  eval (ToggleCellVisibility b next) =
    modify (_ { editorVisible = b }) $> next
  eval (ShareCell next) = pure next

  evalInner :: Natural AnyCellQuery (ComponentDSL CellStateP CellQueryP Slam)
  evalInner q = case q ^? _Query of
    Just q' -> do
      cs <- get
      case fromState' cs of
        Nothing -> halt
        Just cs' -> mapF (eta cs') $ def.eval q'
    Nothing -> halt

  _State :: PrismP AnyCellState s
  _State = clonePrism def._State

  _Query :: forall a. PrismP (AnyCellQuery a) (f a)
  _Query = clonePrism def._Query

  translateF :: Natural (Coproduct CellQuery f) CellQueryP
  translateF = coproduct left (right <<< review _Query)

  fromState' :: CellState AnyCellState -> Maybe (CellState s)
  fromState' cs = map (\is -> mapCS (const is) cs) $ cs.innerState ^? _State

  mapCS :: forall a b. (a -> b) -> CellState a -> CellState b
  mapCS f cs = cs { innerState = f cs.innerState }

  extractState :: CellState s -> CellStateP -> CellState s
  extractState orig cs = case cs.innerState ^? _State of
    Nothing -> orig
    Just innerState -> mapCS (const innerState) cs

  modifyState :: (CellState s -> CellState s) -> CellStateP -> CellStateP
  modifyState f cs = case cs.innerState ^? _State of
    Nothing -> cs
    Just innerState -> mapCS (review _State) $ f (mapCS (const innerState) cs)

  eta :: CellState s -> Natural (HalogenF (CellState s) (Coproduct CellQuery f) Slam) (HalogenF CellStateP CellQueryP Slam)
  eta st = transformHF (mapState (extractState st) modifyState) translateF id

halt :: forall s f g a. Free (HalogenF s f g) a
halt = liftF HaltHF
