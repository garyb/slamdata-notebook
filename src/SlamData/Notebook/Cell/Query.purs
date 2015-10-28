module SlamData.Notebook.Cell.Query where

import Prelude

import Data.Functor.Coproduct (Coproduct())

import Photons.Prism (PrismP())
import Photons.Prism.Coproduct (_Left, _Right)

data ExploreQuery a
data MarkdownQuery a
data QueryQuery a
data SearchQuery a

type AnyCellQuery = Coproduct ExploreQuery (Coproduct MarkdownQuery (Coproduct QueryQuery SearchQuery))

_ExploreQuery :: forall a. PrismP (AnyCellQuery a) (ExploreQuery a)
_ExploreQuery = _Left

_MarkdownQuery :: forall a. PrismP (AnyCellQuery a) (MarkdownQuery a)
_MarkdownQuery = _Right <<< _Left

_QueryQuery :: forall a. PrismP (AnyCellQuery a) (QueryQuery a)
_QueryQuery = _Right <<< _Right <<< _Left

_SearchQuery :: forall a. PrismP (AnyCellQuery a) (SearchQuery a)
_SearchQuery = _Right <<< _Right <<< _Right

