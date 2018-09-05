{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.Simple.List.NonEmpty
    ( NonEmptyCursor
    , makeNonEmptyCursor
    , makeNonEmptyCursorWithSelection
    , singletonNonEmptyCursor
    , rebuildNonEmptyCursor
    , mapNonEmptyCursor
    , nonEmptyCursorElemL
    , nonEmptyCursorSelectPrev
    , nonEmptyCursorSelectNext
    , nonEmptyCursorSelectFirst
    , nonEmptyCursorSelectLast
    , nonEmptyCursorSelection
    , nonEmptyCursorSelectIndex
    , nonEmptyCursorInsert
    , nonEmptyCursorAppend
    , nonEmptyCursorInsertAndSelect
    , nonEmptyCursorAppendAndSelect
    , nonEmptyCursorInsertAtStart
    , nonEmptyCursorAppendAtEnd
    , nonEmptyCursorInsertAtStartAndSelect
    , nonEmptyCursorAppendAtEndAndSelect
    , nonEmptyCursorRemoveElemAndSelectPrev
    , nonEmptyCursorDeleteElemAndSelectNext
    , nonEmptyCursorRemoveElem
    , nonEmptyCursorDeleteElem
    ) where

import Lens.Micro

import Data.List.NonEmpty (NonEmpty(..))

import Cursor.Types

import qualified Cursor.List.NonEmpty as NEC

-- | A 'nonempty list' cursor
type NonEmptyCursor a = NEC.NonEmptyCursor a a

makeNonEmptyCursor :: NonEmpty a -> NonEmptyCursor a
makeNonEmptyCursor = NEC.makeNonEmptyCursor id

makeNonEmptyCursorWithSelection :: Int -> NonEmpty a -> NonEmptyCursor a
makeNonEmptyCursorWithSelection = NEC.makeNonEmptyCursorWithSelection id

singletonNonEmptyCursor :: a -> NonEmptyCursor a
singletonNonEmptyCursor = NEC.singletonNonEmptyCursor

rebuildNonEmptyCursor :: NonEmptyCursor a -> NonEmpty a
rebuildNonEmptyCursor = NEC.rebuildNonEmptyCursor id

mapNonEmptyCursor :: (a -> b) -> NonEmptyCursor a -> NonEmptyCursor b
mapNonEmptyCursor f = NEC.mapNonEmptyCursor f f

nonEmptyCursorElemL :: Lens' (NonEmptyCursor a) a
nonEmptyCursorElemL = NEC.nonEmptyCursorElemL

nonEmptyCursorSelectPrev :: NonEmptyCursor a -> Maybe (NonEmptyCursor a)
nonEmptyCursorSelectPrev = NEC.nonEmptyCursorSelectPrev id id

nonEmptyCursorSelectNext :: NonEmptyCursor a -> Maybe (NonEmptyCursor a)
nonEmptyCursorSelectNext = NEC.nonEmptyCursorSelectNext id id

nonEmptyCursorSelectFirst :: NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorSelectFirst = NEC.nonEmptyCursorSelectFirst id id

nonEmptyCursorSelectLast :: NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorSelectLast = NEC.nonEmptyCursorSelectLast id id

nonEmptyCursorSelection :: NonEmptyCursor a -> Int
nonEmptyCursorSelection = NEC.nonEmptyCursorSelection

nonEmptyCursorSelectIndex :: Int -> NonEmptyCursor a -> Maybe (NonEmptyCursor a)
nonEmptyCursorSelectIndex = NEC.nonEmptyCursorSelectIndex id id

nonEmptyCursorInsert :: a -> NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorInsert = NEC.nonEmptyCursorInsert

nonEmptyCursorAppend :: a -> NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorAppend = NEC.nonEmptyCursorAppend

nonEmptyCursorInsertAndSelect :: a -> NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorInsertAndSelect = NEC.nonEmptyCursorInsertAndSelect id

nonEmptyCursorAppendAndSelect :: a -> NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorAppendAndSelect = NEC.nonEmptyCursorAppendAndSelect id

nonEmptyCursorInsertAtStart :: a -> NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorInsertAtStart = NEC.nonEmptyCursorInsertAtStart

nonEmptyCursorAppendAtEnd :: a -> NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorAppendAtEnd = NEC.nonEmptyCursorAppendAtEnd

nonEmptyCursorInsertAtStartAndSelect ::
       a -> NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorInsertAtStartAndSelect =
    NEC.nonEmptyCursorInsertAtStartAndSelect id id

nonEmptyCursorAppendAtEndAndSelect :: a -> NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorAppendAtEndAndSelect = NEC.nonEmptyCursorAppendAtEndAndSelect id id

nonEmptyCursorRemoveElemAndSelectPrev ::
       NonEmptyCursor a -> Maybe (DeleteOrUpdate (NonEmptyCursor a))
nonEmptyCursorRemoveElemAndSelectPrev =
    NEC.nonEmptyCursorRemoveElemAndSelectPrev id

nonEmptyCursorDeleteElemAndSelectNext ::
       NonEmptyCursor a -> Maybe (DeleteOrUpdate (NonEmptyCursor a))
nonEmptyCursorDeleteElemAndSelectNext =
    NEC.nonEmptyCursorDeleteElemAndSelectNext id

nonEmptyCursorRemoveElem ::
       NonEmptyCursor a -> DeleteOrUpdate (NonEmptyCursor a)
nonEmptyCursorRemoveElem = NEC.nonEmptyCursorRemoveElem id

nonEmptyCursorDeleteElem ::
       NonEmptyCursor a -> DeleteOrUpdate (NonEmptyCursor a)
nonEmptyCursorDeleteElem = NEC.nonEmptyCursorDeleteElem id
