module SlamData.Notebook.Cell.State where

import Prelude

import Data.Either (Either())

import Photons.Prism (PrismP())
import Photons.Prism.Either (_Left, _Right)

type ExploreState = {}
type MarkdownState = {}
type QueryState = {}
type SearchState = {}

type AnyCellState = Either ExploreState (Either MarkdownState (Either QueryState SearchState))

_ExploreState :: PrismP AnyCellState ExploreState
_ExploreState = _Left

_MarkdownState :: PrismP AnyCellState MarkdownState
_MarkdownState = _Right <<< _Left

_QueryState :: PrismP AnyCellState QueryState
_QueryState = _Right <<< _Right <<< _Left

_SearchState :: PrismP AnyCellState SearchState
_SearchState = _Right <<< _Right <<< _Right
