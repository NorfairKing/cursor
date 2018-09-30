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
    , mapMapCursor
    , mapCursorNonEmptyCursorL
    , mapCursorElemL
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
    , mapCursorSearch
    , mapCursorSelectOrAdd
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
    { mapCursorList :: NonEmptyCursor (KeyValueCursor k v k v) (k, v)
    } deriving (Show, Eq, Generic)

instance (Validity k, Validity v) => Validity (MapCursor k v)

makeMapCursor :: NonEmpty (k, v) -> MapCursor k v
makeMapCursor = fromJust . makeMapCursorWithSelection 0 KeySelected

makeMapCursorWithSelection ::
       Int -> KeyValueToggle -> NonEmpty (k, v) -> Maybe (MapCursor k v)
makeMapCursorWithSelection i kvt ne =
    MapCursor <$>
    makeNonEmptyCursorWithSelection
        (uncurry $
         case kvt of
             KeySelected -> makeKeyValueCursorKey
             ValueSelected -> makeKeyValueCursorValue)
        i
        ne

singletonMapCursor :: k -> v -> MapCursor k v
singletonMapCursor k v = makeMapCursor $ (k, v) :| []

rebuildMapCursor :: MapCursor k v -> NonEmpty (k, v)
rebuildMapCursor =
    rebuildNonEmptyCursor (rebuildKeyValueCursor id id) . mapCursorList
mapMapCursor :: (k -> l) -> (v -> w) -> MapCursor k v -> MapCursor l w
mapMapCursor f g =
    mapCursorNonEmptyCursorL %~
    mapNonEmptyCursor (mapKeyValueCursor f g f g) (\(k, v) -> (f k, g v))


mapCursorNonEmptyCursorL ::
       Lens (MapCursor k v) (MapCursor l w) (NonEmptyCursor (KeyValueCursor k v k v) ( k
                                                                                     , v)) (NonEmptyCursor (KeyValueCursor l w l w) ( l
                                                                                                                                    , w))
mapCursorNonEmptyCursorL =
    lens mapCursorList $ \mc ne -> mc {mapCursorList = ne}

mapCursorElemL :: Lens' (MapCursor k v) (KeyValueCursor k v k v)
mapCursorElemL = mapCursorNonEmptyCursorL . nonEmptyCursorElemL

mapCursorSelectKey :: MapCursor k v -> MapCursor k v
mapCursorSelectKey = mapCursorElemL %~ keyValueCursorSelectKey id id

mapCursorSelectValue :: MapCursor k v -> MapCursor k v
mapCursorSelectValue = mapCursorElemL %~ keyValueCursorSelectValue id id

mapCursorToggleSelected :: MapCursor k v -> MapCursor k v
mapCursorToggleSelected =
    mapCursorElemL %~ keyValueCursorToggleSelected id id id id

mapCursorSelectPrev :: MapCursor k v -> Maybe (MapCursor k v)
mapCursorSelectPrev =
    mapCursorNonEmptyCursorL $ nonEmptyCursorSelectPrev rebuild make

mapCursorSelectNext :: MapCursor k v -> Maybe (MapCursor k v)
mapCursorSelectNext =
    mapCursorNonEmptyCursorL $ nonEmptyCursorSelectNext rebuild make

mapCursorSelectFirst :: MapCursor k v -> MapCursor k v
mapCursorSelectFirst =
    mapCursorNonEmptyCursorL %~ (nonEmptyCursorSelectFirst rebuild make)

mapCursorSelectLast :: MapCursor k v -> MapCursor k v
mapCursorSelectLast =
    mapCursorNonEmptyCursorL %~ (nonEmptyCursorSelectLast rebuild make)

mapCursorSelection :: MapCursor k v -> Int
mapCursorSelection = nonEmptyCursorSelection . mapCursorList

mapCursorSelectIndex :: Int -> MapCursor k v -> Maybe (MapCursor k v)
mapCursorSelectIndex i =
    mapCursorNonEmptyCursorL (nonEmptyCursorSelectIndex rebuild make i)

mapCursorInsert :: k -> v -> MapCursor k v -> MapCursor k v
mapCursorInsert k v = mapCursorNonEmptyCursorL %~ (nonEmptyCursorInsert (k, v))

mapCursorAppend :: k -> v -> MapCursor k v -> MapCursor k v
mapCursorAppend k v = mapCursorNonEmptyCursorL %~ (nonEmptyCursorAppend (k, v))

mapCursorInsertAndSelect :: k -> v -> MapCursor k v -> MapCursor k v
mapCursorInsertAndSelect k v =
    mapCursorNonEmptyCursorL %~
    (nonEmptyCursorInsertAndSelect rebuild (make (k, v)))

mapCursorAppendAndSelect :: k -> v -> MapCursor k v -> MapCursor k v
mapCursorAppendAndSelect k v =
    mapCursorNonEmptyCursorL %~
    (nonEmptyCursorAppendAndSelect rebuild (make (k, v)))

mapCursorRemoveElemAndSelectPrev ::
       MapCursor k v -> Maybe (DeleteOrUpdate (MapCursor k v))
mapCursorRemoveElemAndSelectPrev =
    focusPossibleDeleteOrUpdate mapCursorNonEmptyCursorL $
    nonEmptyCursorRemoveElemAndSelectPrev make

mapCursorDeleteElemAndSelectNext ::
       MapCursor k v -> Maybe (DeleteOrUpdate (MapCursor k v))
mapCursorDeleteElemAndSelectNext =
    focusPossibleDeleteOrUpdate mapCursorNonEmptyCursorL $
    nonEmptyCursorDeleteElemAndSelectNext make

mapCursorRemoveElem :: MapCursor k v -> DeleteOrUpdate (MapCursor k v)
mapCursorRemoveElem = mapCursorNonEmptyCursorL $ nonEmptyCursorRemoveElem make

mapCursorDeleteElem :: MapCursor k v -> DeleteOrUpdate (MapCursor k v)
mapCursorDeleteElem = mapCursorNonEmptyCursorL $ nonEmptyCursorDeleteElem make

mapCursorSearch :: (k -> v -> Bool) -> MapCursor k v -> Maybe (MapCursor k v)
mapCursorSearch p =
    mapCursorNonEmptyCursorL $
    nonEmptyCursorSearch rebuild make (uncurry p . rebuild)

mapCursorSelectOrAdd ::
       (k -> v -> Bool)
    -> KeyValueCursor k v k v
    -> MapCursor k v
    -> MapCursor k v
mapCursorSelectOrAdd p kvc =
    mapCursorNonEmptyCursorL %~
    nonEmptyCursorSelectOrAdd rebuild make (uncurry p . rebuild) kvc

rebuild :: KeyValueCursor k v k v -> (k, v)
rebuild = rebuildKeyValueCursor id id

make :: (k, v) -> KeyValueCursor k v k v
make = uncurry makeKeyValueCursorKey
