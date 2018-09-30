{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.Simple.Map
    ( MapCursor(..)
    , makeMapCursor
    , makeMapCursorWithSelection
    , MC.singletonMapCursor
    , rebuildMapCursor
    , mapMapCursor
    , MC.mapCursorNonEmptyCursorL
    , MC.mapCursorElemL
    , mapCursorSelectKey
    , mapCursorSelectValue
    , mapCursorToggleSelected
    , mapCursorSelectPrev
    , mapCursorSelectNext
    , mapCursorSelectFirst
    , mapCursorSelectLast
    , MC.mapCursorSelection
    , mapCursorSelectIndex
    , MC.mapCursorInsert
    , MC.mapCursorAppend
    , mapCursorInsertAndSelect
    , mapCursorAppendAndSelect
    , mapCursorRemoveElemAndSelectPrev
    , mapCursorDeleteElemAndSelectNext
    , mapCursorRemoveElem
    , mapCursorDeleteElem
    , mapCursorSearch
    , mapCursorSelectOrAdd
    , module Cursor.Simple.Map.KeyValue
    ) where

import GHC.Generics (Generic)

import Data.Validity
import Data.Validity.Tree ()

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe

import Lens.Micro

import Cursor.List.NonEmpty
import qualified Cursor.Map as MC
import Cursor.Simple.Map.KeyValue
import Cursor.Types

type MapCursor k v = MC.MapCursor k v k v

makeMapCursor :: NonEmpty (k, v) -> MapCursor k v
makeMapCursor = MC.makeMapCursor id

makeMapCursorWithSelection :: Int -> NonEmpty (k, v) -> Maybe (MapCursor k v)
makeMapCursorWithSelection = MC.makeMapCursorWithSelection id

rebuildMapCursor :: MapCursor k v -> NonEmpty (k, v)
rebuildMapCursor = MC.rebuildMapCursor id id

mapMapCursor :: (k -> l) -> (v -> w) -> MapCursor k v -> MapCursor l w
mapMapCursor f g = MC.mapMapCursor f g f g

mapCursorSelectKey :: MapCursor k v -> MapCursor k v
mapCursorSelectKey = MC.mapCursorSelectKey id id

mapCursorSelectValue :: MapCursor k v -> MapCursor k v
mapCursorSelectValue = MC.mapCursorSelectValue id id

mapCursorToggleSelected :: MapCursor k v -> MapCursor k v
mapCursorToggleSelected = MC.mapCursorToggleSelected id id id id

mapCursorSelectPrev :: MapCursor k v -> Maybe (MapCursor k v)
mapCursorSelectPrev = MC.mapCursorSelectPrev id id id

mapCursorSelectNext :: MapCursor k v -> Maybe (MapCursor k v)
mapCursorSelectNext = MC.mapCursorSelectNext id id id

mapCursorSelectFirst :: MapCursor k v -> MapCursor k v
mapCursorSelectFirst = MC.mapCursorSelectFirst id id id

mapCursorSelectLast :: MapCursor k v -> MapCursor k v
mapCursorSelectLast = MC.mapCursorSelectLast id id id

mapCursorSelectIndex :: Int -> MapCursor k v -> Maybe (MapCursor k v)
mapCursorSelectIndex = MC.mapCursorSelectIndex id id id

mapCursorInsertAndSelect :: k -> v -> MapCursor k v -> MapCursor k v
mapCursorInsertAndSelect = MC.mapCursorInsertAndSelect id id

mapCursorAppendAndSelect :: k -> v -> MapCursor k v -> MapCursor k v
mapCursorAppendAndSelect = MC.mapCursorAppendAndSelect id id

mapCursorRemoveElemAndSelectPrev ::
       MapCursor k v -> Maybe (DeleteOrUpdate (MapCursor k v))
mapCursorRemoveElemAndSelectPrev = MC.mapCursorRemoveElemAndSelectPrev id

mapCursorDeleteElemAndSelectNext ::
       MapCursor k v -> Maybe (DeleteOrUpdate (MapCursor k v))
mapCursorDeleteElemAndSelectNext = MC.mapCursorDeleteElemAndSelectNext id

mapCursorRemoveElem :: MapCursor k v -> DeleteOrUpdate (MapCursor k v)
mapCursorRemoveElem = MC.mapCursorRemoveElem id

mapCursorDeleteElem :: MapCursor k v -> DeleteOrUpdate (MapCursor k v)
mapCursorDeleteElem = MC.mapCursorDeleteElem id

mapCursorSearch :: (k -> v -> Bool) -> MapCursor k v -> Maybe (MapCursor k v)
mapCursorSearch = MC.mapCursorSearch id id id

mapCursorSelectOrAdd ::
       (k -> v -> Bool) -> KeyValueCursor k v -> MapCursor k v -> MapCursor k v
mapCursorSelectOrAdd = MC.mapCursorSelectOrAdd id id id
