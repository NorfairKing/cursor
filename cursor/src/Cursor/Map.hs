{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.Map
    ( MapCursor(..)
    , makeMapCursor
    , makeMapCursorWithSelection
    , singletonMapCursor
    , rebuildMapCursor
    , mapCursorNonEmptyCursorL
    , mapCursorElemL
    , mapCursorElemKeyL
    , mapCursorElemValueL
    , mapCursorSelectKey
    , mapCursorSelectValue
    , mapCursorToggleSelected
    , mapCursorSelectPrev
    , mapCursorSelectNext
    , mapCursorSelectFirst
    , mapCursorSelectLast
    , mapCursorSelection
    , mapCursorSelectIndex
    , mapCursorInsert
    , mapCursorAppend
    , mapCursorInsertAndSelect
    , mapCursorAppendAndSelect
    , mapCursorRemoveElemAndSelectPrev
    , mapCursorDeleteElemAndSelectNext
    , mapCursorRemoveElem
    , mapCursorDeleteElem
    , module Cursor.Map.KeyValue
    ) where

import GHC.Generics (Generic)

import Data.Validity
import Data.Validity.Tree ()

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe

import Lens.Micro

import Cursor.List.NonEmpty
import Cursor.Map.KeyValue
import Cursor.Types

newtype MapCursor k v = MapCursor
    { mapCursorList :: NonEmptyCursor (KeyValueCursor k v) (k, v)
    } deriving (Show, Eq, Generic)

instance (Validity k, Validity v) => Validity (MapCursor k v)

instance Functor (MapCursor k) where
    fmap f = mapCursorNonEmptyCursorL %~ mapNonEmptyCursor (fmap f) (fmap f)

makeMapCursor :: NonEmpty (k, v) -> MapCursor k v
makeMapCursor = fromJust . makeMapCursorWithSelection 0

makeMapCursorWithSelection :: Int -> NonEmpty (k, v) -> Maybe (MapCursor k v)
makeMapCursorWithSelection i ne =
    MapCursor <$>
    makeNonEmptyCursorWithSelection (uncurry makeKeyValueCursor) i ne

singletonMapCursor :: k -> v -> MapCursor k v
singletonMapCursor k v = makeMapCursor $ (k, v) :| []

rebuildMapCursor :: MapCursor k v -> NonEmpty (k, v)
rebuildMapCursor = rebuildNonEmptyCursor rebuildKeyValueCursor . mapCursorList

mapCursorNonEmptyCursorL ::
       Lens (MapCursor k v) (MapCursor l w) (NonEmptyCursor (KeyValueCursor k v) ( k
                                                                                 , v)) (NonEmptyCursor (KeyValueCursor l w) ( l
                                                                                                                            , w))
mapCursorNonEmptyCursorL =
    lens mapCursorList $ \mc ne -> mc {mapCursorList = ne}

mapCursorElemL :: Lens' (MapCursor k v) (KeyValueCursor k v)
mapCursorElemL = mapCursorNonEmptyCursorL . nonEmptyCursorElemL

mapCursorElemKeyL :: Lens' (MapCursor k v) k
mapCursorElemKeyL = mapCursorElemL . keyValueCursorKeyL

mapCursorElemValueL :: Lens' (MapCursor k v) v
mapCursorElemValueL = mapCursorElemL . keyValueCursorValueL

mapCursorSelectKey :: MapCursor k v -> MapCursor k v
mapCursorSelectKey = mapCursorElemL %~ keyValueCursorSelectKey

mapCursorSelectValue :: MapCursor k v -> MapCursor k v
mapCursorSelectValue = mapCursorElemL %~ keyValueCursorSelectValue

mapCursorToggleSelected :: MapCursor k v -> MapCursor k v
mapCursorToggleSelected = mapCursorElemL %~ keyValueCursorToggleSelected

mapCursorSelectPrev :: MapCursor k v -> Maybe (MapCursor k v)
mapCursorSelectPrev =
    mapCursorNonEmptyCursorL $
    nonEmptyCursorSelectPrev rebuildKeyValueCursor (uncurry makeKeyValueCursor)

mapCursorSelectNext :: MapCursor k v -> Maybe (MapCursor k v)
mapCursorSelectNext =
    mapCursorNonEmptyCursorL $
    nonEmptyCursorSelectNext rebuildKeyValueCursor (uncurry makeKeyValueCursor)

mapCursorSelectFirst :: MapCursor k v -> MapCursor k v
mapCursorSelectFirst =
    mapCursorNonEmptyCursorL %~
    (nonEmptyCursorSelectFirst
         rebuildKeyValueCursor
         (uncurry makeKeyValueCursor))

mapCursorSelectLast :: MapCursor k v -> MapCursor k v
mapCursorSelectLast =
    mapCursorNonEmptyCursorL %~
    (nonEmptyCursorSelectLast rebuildKeyValueCursor (uncurry makeKeyValueCursor))

mapCursorSelection :: MapCursor k v -> Int
mapCursorSelection = nonEmptyCursorSelection . mapCursorList

mapCursorSelectIndex :: Int -> MapCursor k v -> Maybe (MapCursor k v)
mapCursorSelectIndex i =
    mapCursorNonEmptyCursorL
        (nonEmptyCursorSelectIndex
             rebuildKeyValueCursor
             (uncurry makeKeyValueCursor)
             i)

mapCursorInsert :: k -> v -> MapCursor k v -> MapCursor k v
mapCursorInsert k v = mapCursorNonEmptyCursorL %~ (nonEmptyCursorInsert (k, v))

mapCursorAppend :: k -> v -> MapCursor k v -> MapCursor k v
mapCursorAppend k v = mapCursorNonEmptyCursorL %~ (nonEmptyCursorAppend (k, v))

mapCursorInsertAndSelect :: k -> v -> MapCursor k v -> MapCursor k v
mapCursorInsertAndSelect k v =
    mapCursorNonEmptyCursorL %~
    (nonEmptyCursorInsertAndSelect rebuildKeyValueCursor $
     makeKeyValueCursor k v)

mapCursorAppendAndSelect :: k -> v -> MapCursor k v -> MapCursor k v
mapCursorAppendAndSelect k v =
    mapCursorNonEmptyCursorL %~
    (nonEmptyCursorAppendAndSelect rebuildKeyValueCursor $
     makeKeyValueCursor k v)

mapCursorRemoveElemAndSelectPrev ::
       MapCursor k v -> Maybe (DeleteOrUpdate (MapCursor k v))
mapCursorRemoveElemAndSelectPrev =
    focusPossibleDeleteOrUpdate mapCursorNonEmptyCursorL $
    nonEmptyCursorRemoveElemAndSelectPrev (uncurry makeKeyValueCursor)

mapCursorDeleteElemAndSelectNext ::
       MapCursor k v -> Maybe (DeleteOrUpdate (MapCursor k v))
mapCursorDeleteElemAndSelectNext =
    focusPossibleDeleteOrUpdate mapCursorNonEmptyCursorL $
    nonEmptyCursorDeleteElemAndSelectNext (uncurry makeKeyValueCursor)

mapCursorRemoveElem :: MapCursor k v -> DeleteOrUpdate (MapCursor k v)
mapCursorRemoveElem =
    mapCursorNonEmptyCursorL $
    nonEmptyCursorRemoveElem (uncurry makeKeyValueCursor)

mapCursorDeleteElem :: MapCursor k v -> DeleteOrUpdate (MapCursor k v)
mapCursorDeleteElem =
    mapCursorNonEmptyCursorL $
    nonEmptyCursorDeleteElem (uncurry makeKeyValueCursor)
