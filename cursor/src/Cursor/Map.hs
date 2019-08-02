{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.Map
  ( MapCursor(..)
  , makeMapCursor
  , makeMapCursorWithSelection
  , singletonMapCursorKey
  , singletonMapCursorValue
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
  , mapCursorInsertAndSelectKey
  , mapCursorAppendAndSelectKey
  , mapCursorInsertAndSelectValue
  , mapCursorAppendAndSelectValue
  , mapCursorRemoveElemAndSelectPrev
  , mapCursorDeleteElemAndSelectNext
  , mapCursorRemoveElem
  , mapCursorDeleteElem
  , mapCursorSearch
  , mapCursorSelectOrAdd
  , traverseMapCursor
  , foldMapCursor
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

newtype MapCursor kc vc k v =
  MapCursor
    { mapCursorList :: NonEmptyCursor (KeyValueCursor kc vc k v) (k, v)
    }
  deriving (Show, Eq, Generic)

instance (Validity kc, Validity vc, Validity k, Validity v) =>
         Validity (MapCursor kc vc k v)

makeMapCursor :: (k -> kc) -> NonEmpty (k, v) -> MapCursor kc vc k v
makeMapCursor h = fromJust . makeMapCursorWithSelection h 0

makeMapCursorWithSelection ::
     (k -> kc) -> Int -> NonEmpty (k, v) -> Maybe (MapCursor kc vc k v)
makeMapCursorWithSelection h i ne =
  MapCursor <$>
  makeNonEmptyCursorWithSelection
    (\(k, v) -> makeKeyValueCursorKey (h k) v)
    i
    ne

singletonMapCursorKey :: kc -> v -> MapCursor kc vc k v
singletonMapCursorKey kc v =
  MapCursor
    {mapCursorList = singletonNonEmptyCursor $ makeKeyValueCursorKey kc v}

singletonMapCursorValue :: k -> vc -> MapCursor kc vc k v
singletonMapCursorValue k vc =
  MapCursor
    {mapCursorList = singletonNonEmptyCursor $ makeKeyValueCursorValue k vc}

rebuildMapCursor ::
     (kc -> k) -> (vc -> v) -> MapCursor kc vc k v -> NonEmpty (k, v)
rebuildMapCursor f g =
  rebuildNonEmptyCursor (rebuildKeyValueCursor f g) . mapCursorList

mapMapCursor ::
     (kc -> lc)
  -> (vc -> wc)
  -> (k -> l)
  -> (v -> w)
  -> MapCursor kc vc k v
  -> MapCursor lc wc l w
mapMapCursor a b c d =
  mapCursorNonEmptyCursorL %~
  mapNonEmptyCursor (mapKeyValueCursor a b c d) (\(k, v) -> (c k, d v))

mapCursorNonEmptyCursorL ::
     Lens (MapCursor kc vc k v) (MapCursor lc wc l w) (NonEmptyCursor (KeyValueCursor kc vc k v) ( k
                                                                                                 , v)) (NonEmptyCursor (KeyValueCursor lc wc l w) ( l
                                                                                                                                                  , w))
mapCursorNonEmptyCursorL =
  lens mapCursorList $ \mc ne -> mc {mapCursorList = ne}

mapCursorElemL :: Lens' (MapCursor kc vc k v) (KeyValueCursor kc vc k v)
mapCursorElemL = mapCursorNonEmptyCursorL . nonEmptyCursorElemL

mapCursorSelectKey ::
     (k -> kc) -> (vc -> v) -> MapCursor kc vc k v -> MapCursor kc vc k v
mapCursorSelectKey g h = mapCursorElemL %~ keyValueCursorSelectKey g h

mapCursorSelectValue ::
     (kc -> k) -> (v -> vc) -> MapCursor kc vc k v -> MapCursor kc vc k v
mapCursorSelectValue f i = mapCursorElemL %~ keyValueCursorSelectValue f i

mapCursorToggleSelected ::
     (kc -> k)
  -> (k -> kc)
  -> (vc -> v)
  -> (v -> vc)
  -> MapCursor kc vc k v
  -> MapCursor kc vc k v
mapCursorToggleSelected f g h i =
  mapCursorElemL %~ keyValueCursorToggleSelected f g h i

mapCursorSelectPrev ::
     (kc -> k)
  -> (k -> kc)
  -> (vc -> v)
  -> MapCursor kc vc k v
  -> Maybe (MapCursor kc vc k v)
mapCursorSelectPrev f g h =
  mapCursorNonEmptyCursorL $ nonEmptyCursorSelectPrev (rebuild f h) (make g)

mapCursorSelectNext ::
     (kc -> k)
  -> (k -> kc)
  -> (vc -> v)
  -> MapCursor kc vc k v
  -> Maybe (MapCursor kc vc k v)
mapCursorSelectNext f g h =
  mapCursorNonEmptyCursorL $ nonEmptyCursorSelectNext (rebuild f h) (make g)

mapCursorSelectFirst ::
     (kc -> k)
  -> (k -> kc)
  -> (vc -> v)
  -> MapCursor kc vc k v
  -> MapCursor kc vc k v
mapCursorSelectFirst f g h =
  mapCursorNonEmptyCursorL %~ (nonEmptyCursorSelectFirst (rebuild f h) (make g))

mapCursorSelectLast ::
     (kc -> k)
  -> (k -> kc)
  -> (vc -> v)
  -> MapCursor kc vc k v
  -> MapCursor kc vc k v
mapCursorSelectLast f g h =
  mapCursorNonEmptyCursorL %~ (nonEmptyCursorSelectLast (rebuild f h) (make g))

mapCursorSelection :: MapCursor kc vc k v -> Int
mapCursorSelection = nonEmptyCursorSelection . mapCursorList

mapCursorSelectIndex ::
     (kc -> k)
  -> (k -> kc)
  -> (vc -> v)
  -> Int
  -> MapCursor kc vc k v
  -> Maybe (MapCursor kc vc k v)
mapCursorSelectIndex f g h i =
  mapCursorNonEmptyCursorL (nonEmptyCursorSelectIndex (rebuild f h) (make g) i)

mapCursorInsert :: k -> v -> MapCursor kc vc k v -> MapCursor kc vc k v
mapCursorInsert k v = mapCursorNonEmptyCursorL %~ (nonEmptyCursorInsert (k, v))

mapCursorAppend :: k -> v -> MapCursor kc vc k v -> MapCursor kc vc k v
mapCursorAppend k v = mapCursorNonEmptyCursorL %~ (nonEmptyCursorAppend (k, v))

mapCursorInsertAndSelectKey ::
     (kc -> k)
  -> (vc -> v)
  -> kc
  -> v
  -> MapCursor kc vc k v
  -> MapCursor kc vc k v
mapCursorInsertAndSelectKey f h kc v =
  mapCursorNonEmptyCursorL %~
  (nonEmptyCursorInsertAndSelect (rebuild f h) (makeKeyValueCursorKey kc v))

mapCursorAppendAndSelectKey ::
     (kc -> k)
  -> (vc -> v)
  -> kc
  -> v
  -> MapCursor kc vc k v
  -> MapCursor kc vc k v
mapCursorAppendAndSelectKey f h kc v =
  mapCursorNonEmptyCursorL %~
  (nonEmptyCursorAppendAndSelect (rebuild f h) (makeKeyValueCursorKey kc v))

mapCursorInsertAndSelectValue ::
     (kc -> k)
  -> (vc -> v)
  -> k
  -> vc
  -> MapCursor kc vc k v
  -> MapCursor kc vc k v
mapCursorInsertAndSelectValue f h k vc =
  mapCursorNonEmptyCursorL %~
  (nonEmptyCursorInsertAndSelect (rebuild f h) (makeKeyValueCursorValue k vc))

mapCursorAppendAndSelectValue ::
     (kc -> k)
  -> (vc -> v)
  -> k
  -> vc
  -> MapCursor kc vc k v
  -> MapCursor kc vc k v
mapCursorAppendAndSelectValue f h k vc =
  mapCursorNonEmptyCursorL %~
  (nonEmptyCursorAppendAndSelect (rebuild f h) (makeKeyValueCursorValue k vc))

mapCursorRemoveElemAndSelectPrev ::
     (k -> kc)
  -> MapCursor kc vc k v
  -> Maybe (DeleteOrUpdate (MapCursor kc vc k v))
mapCursorRemoveElemAndSelectPrev g =
  focusPossibleDeleteOrUpdate mapCursorNonEmptyCursorL $
  nonEmptyCursorRemoveElemAndSelectPrev (make g)

mapCursorDeleteElemAndSelectNext ::
     (k -> kc)
  -> MapCursor kc vc k v
  -> Maybe (DeleteOrUpdate (MapCursor kc vc k v))
mapCursorDeleteElemAndSelectNext g =
  focusPossibleDeleteOrUpdate mapCursorNonEmptyCursorL $
  nonEmptyCursorDeleteElemAndSelectNext (make g)

mapCursorRemoveElem ::
     (k -> kc) -> MapCursor kc vc k v -> DeleteOrUpdate (MapCursor kc vc k v)
mapCursorRemoveElem g =
  mapCursorNonEmptyCursorL $ nonEmptyCursorRemoveElem (make g)

mapCursorDeleteElem ::
     (k -> kc) -> MapCursor kc vc k v -> DeleteOrUpdate (MapCursor kc vc k v)
mapCursorDeleteElem g =
  mapCursorNonEmptyCursorL $ nonEmptyCursorDeleteElem (make g)

mapCursorSearch ::
     (kc -> k)
  -> (k -> kc)
  -> (vc -> v)
  -> (k -> v -> Bool)
  -> MapCursor kc vc k v
  -> Maybe (MapCursor kc vc k v)
mapCursorSearch f g h p =
  mapCursorNonEmptyCursorL $
  nonEmptyCursorSearch (rebuild f h) (make g) (uncurry p . rebuild f h)

mapCursorSelectOrAdd ::
     (kc -> k)
  -> (k -> kc)
  -> (vc -> v)
  -> (k -> v -> Bool)
  -> KeyValueCursor kc vc k v
  -> MapCursor kc vc k v
  -> MapCursor kc vc k v
mapCursorSelectOrAdd f g h p kvc =
  mapCursorNonEmptyCursorL %~
  nonEmptyCursorSelectOrAdd (rebuild f h) (make g) (uncurry p . rebuild f h) kvc

rebuild :: (kc -> k) -> (vc -> v) -> KeyValueCursor kc vc k v -> (k, v)
rebuild f h = rebuildKeyValueCursor f h

make :: (k -> kc) -> (k, v) -> KeyValueCursor kc vc k v
make g (k, v) = makeKeyValueCursorKey (g k) v

traverseMapCursor ::
     ([(k, v)] -> KeyValueCursor kc vc k v -> [(k, v)] -> f c)
  -> MapCursor kc vc k v
  -> f c
traverseMapCursor combFunc = foldNonEmptyCursor combFunc . mapCursorList

foldMapCursor ::
     ([(k, v)] -> KeyValueCursor kc vc k v -> [(k, v)] -> c)
  -> MapCursor kc vc k v
  -> c
foldMapCursor combFunc = foldNonEmptyCursor combFunc . mapCursorList
