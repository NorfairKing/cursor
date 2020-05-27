{-# LANGUAGE TypeFamilies #-}

module Cursor.Simple.List.NonEmpty
  ( NonEmptyCursor,
    NEC.nonEmptyCursorPrev,
    NEC.nonEmptyCursorCurrent,
    NEC.nonEmptyCursorNext,
    makeNonEmptyCursor,
    makeNonEmptyCursorWithSelection,
    NEC.singletonNonEmptyCursor,
    rebuildNonEmptyCursor,
    mapNonEmptyCursor,
    NEC.nonEmptyCursorElemL,
    nonEmptyCursorSelectPrev,
    nonEmptyCursorSelectNext,
    nonEmptyCursorSelectFirst,
    nonEmptyCursorSelectLast,
    NEC.nonEmptyCursorSelection,
    nonEmptyCursorSelectIndex,
    NEC.nonEmptyCursorInsert,
    NEC.nonEmptyCursorAppend,
    nonEmptyCursorInsertAndSelect,
    nonEmptyCursorAppendAndSelect,
    NEC.nonEmptyCursorInsertAtStart,
    NEC.nonEmptyCursorAppendAtEnd,
    nonEmptyCursorInsertAtStartAndSelect,
    nonEmptyCursorAppendAtEndAndSelect,
    nonEmptyCursorRemoveElemAndSelectPrev,
    nonEmptyCursorDeleteElemAndSelectNext,
    nonEmptyCursorRemoveElem,
    nonEmptyCursorDeleteElem,
    nonEmptyCursorSearch,
    nonEmptyCursorSelectOrAdd,
  )
where

import qualified Cursor.List.NonEmpty as NEC
import Cursor.Types
import Data.List.NonEmpty (NonEmpty (..))

-- | A 'nonempty list' cursor
type NonEmptyCursor a = NEC.NonEmptyCursor a a

makeNonEmptyCursor :: NonEmpty a -> NonEmptyCursor a
makeNonEmptyCursor = NEC.makeNonEmptyCursor id

makeNonEmptyCursorWithSelection :: Int -> NonEmpty a -> Maybe (NonEmptyCursor a)
makeNonEmptyCursorWithSelection = NEC.makeNonEmptyCursorWithSelection id

rebuildNonEmptyCursor :: NonEmptyCursor a -> NonEmpty a
rebuildNonEmptyCursor = NEC.rebuildNonEmptyCursor id

mapNonEmptyCursor :: (a -> b) -> NonEmptyCursor a -> NonEmptyCursor b
mapNonEmptyCursor f = NEC.mapNonEmptyCursor f f

nonEmptyCursorSelectPrev :: NonEmptyCursor a -> Maybe (NonEmptyCursor a)
nonEmptyCursorSelectPrev = NEC.nonEmptyCursorSelectPrev id id

nonEmptyCursorSelectNext :: NonEmptyCursor a -> Maybe (NonEmptyCursor a)
nonEmptyCursorSelectNext = NEC.nonEmptyCursorSelectNext id id

nonEmptyCursorSelectFirst :: NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorSelectFirst = NEC.nonEmptyCursorSelectFirst id id

nonEmptyCursorSelectLast :: NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorSelectLast = NEC.nonEmptyCursorSelectLast id id

nonEmptyCursorSelectIndex :: Int -> NonEmptyCursor a -> Maybe (NonEmptyCursor a)
nonEmptyCursorSelectIndex = NEC.nonEmptyCursorSelectIndex id id

nonEmptyCursorInsertAndSelect :: a -> NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorInsertAndSelect = NEC.nonEmptyCursorInsertAndSelect id

nonEmptyCursorAppendAndSelect :: a -> NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorAppendAndSelect = NEC.nonEmptyCursorAppendAndSelect id

nonEmptyCursorInsertAtStartAndSelect :: a -> NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorInsertAtStartAndSelect = NEC.nonEmptyCursorInsertAtStartAndSelect id id

nonEmptyCursorAppendAtEndAndSelect :: a -> NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorAppendAtEndAndSelect = NEC.nonEmptyCursorAppendAtEndAndSelect id id

nonEmptyCursorRemoveElemAndSelectPrev ::
  NonEmptyCursor a -> Maybe (DeleteOrUpdate (NonEmptyCursor a))
nonEmptyCursorRemoveElemAndSelectPrev = NEC.nonEmptyCursorRemoveElemAndSelectPrev id

nonEmptyCursorDeleteElemAndSelectNext ::
  NonEmptyCursor a -> Maybe (DeleteOrUpdate (NonEmptyCursor a))
nonEmptyCursorDeleteElemAndSelectNext = NEC.nonEmptyCursorDeleteElemAndSelectNext id

nonEmptyCursorRemoveElem :: NonEmptyCursor a -> DeleteOrUpdate (NonEmptyCursor a)
nonEmptyCursorRemoveElem = NEC.nonEmptyCursorRemoveElem id

nonEmptyCursorDeleteElem :: NonEmptyCursor a -> DeleteOrUpdate (NonEmptyCursor a)
nonEmptyCursorDeleteElem = NEC.nonEmptyCursorDeleteElem id

nonEmptyCursorSearch :: (a -> Bool) -> NonEmptyCursor a -> Maybe (NonEmptyCursor a)
nonEmptyCursorSearch = NEC.nonEmptyCursorSearch id id

nonEmptyCursorSelectOrAdd :: (a -> Bool) -> a -> NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorSelectOrAdd = NEC.nonEmptyCursorSelectOrAdd id id
