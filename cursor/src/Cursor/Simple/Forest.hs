{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.Simple.Forest
    ( ForestCursor
    , makeForestCursor
    , rebuildForestCursor
    , FC.drawForestCursor
    , mapForestCursor
    , FC.forestCursorListCursorL
    , FC.forestCursorSelectedTreeL
    , forestCursorSelectPrevTreeCursor
    , forestCursorSelectNextTreeCursor
    , forestCursorSelectFirstTreeCursor
    , forestCursorSelectLastTreeCursor
    , forestCursorSelectPrev
    , forestCursorSelectNext
    , forestCursorSelectPrevOnSameLevel
    , forestCursorSelectNextOnSameLevel
    , forestCursorSelectFirst
    , forestCursorSelectLast
    , forestCursorSelectBelowAtPos
    , forestCursorSelectBelowAtStart
    , forestCursorSelectBelowAtEnd
    , FC.forestCursorSelection
    , forestCursorSelectIndex
    , FC.forestCursorInsertEntireTree
    , forestCursorInsertAndSelectTreeCursor
    , FC.forestCursorAppendEntireTree
    , forestCursorAppendAndSelectTreeCursor
    , FC.forestCursorInsertTree
    , FC.forestCursorAppendTree
    , forestCursorInsertAndSelectTree
    , forestCursorAppendAndSelectTree
    , FC.forestCursorInsert
    , FC.forestCursorAppend
    , forestCursorInsertAndSelect
    , forestCursorAppendAndSelect
    , FC.forestCursorAddChildTreeToNodeAtPos
    , FC.forestCursorAddChildTreeToNodeAtStart
    , FC.forestCursorAddChildTreeToNodeAtEnd
    , FC.forestCursorAddChildToNodeAtPos
    , FC.forestCursorAddChildToNodeAtStart
    , FC.forestCursorAddChildToNodeAtEnd
    , forestCursorRemoveElemAndSelectPrev
    , forestCursorDeleteElemAndSelectNext
    , forestCursorRemoveElem
    , forestCursorDeleteElem
    , forestCursorRemoveSubTreeAndSelectPrev
    , forestCursorDeleteSubTreeAndSelectNext
    , forestCursorRemoveSubTree
    , forestCursorDeleteSubTree
    , forestCursorAddRoot
    , FC.forestCursorSwapPrev
    , FC.forestCursorSwapNext
    , forestCursorPromoteElem
    , forestCursorPromoteSubTree
    , forestCursorDemoteElem
    , forestCursorDemoteSubTree
    , FC.forestCursorDemoteElemUnder
    , FC.forestCursorDemoteSubTreeUnder
    , FC.CTree(..)
    , FC.makeCTree
    , FC.cTree
    , FC.rebuildCTree
    , FC.CForest(..)
    , FC.makeCForest
    , FC.cForest
    , FC.rebuildCForest
    ) where

import Data.Validity.Tree ()

import Data.List.NonEmpty (NonEmpty)
import Data.Tree

import Lens.Micro

import qualified Cursor.Forest as FC
import Cursor.List.NonEmpty
import Cursor.Simple.Tree
import Cursor.Types

type ForestCursor a = FC.ForestCursor a a

makeForestCursor :: NonEmpty (CTree a) -> ForestCursor a
makeForestCursor = FC.makeForestCursor id

rebuildForestCursor :: ForestCursor a -> NonEmpty (CTree a)
rebuildForestCursor = FC.rebuildForestCursor id

mapForestCursor :: (a -> b) -> ForestCursor a -> ForestCursor b
mapForestCursor f = FC.mapForestCursor f f

forestCursorSelectPrevTreeCursor :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectPrevTreeCursor = FC.forestCursorSelectPrevTreeCursor id id

forestCursorSelectNextTreeCursor :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectNextTreeCursor = FC.forestCursorSelectNextTreeCursor id id

forestCursorSelectFirstTreeCursor :: ForestCursor a -> ForestCursor a
forestCursorSelectFirstTreeCursor = FC.forestCursorSelectFirstTreeCursor id id

forestCursorSelectLastTreeCursor :: ForestCursor a -> ForestCursor a
forestCursorSelectLastTreeCursor = FC.forestCursorSelectLastTreeCursor id id

forestCursorSelectNext :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectNext = FC.forestCursorSelectNext id id

forestCursorSelectPrev :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectPrev = FC.forestCursorSelectPrev id id

forestCursorSelectNextOnSameLevel :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectNextOnSameLevel = FC.forestCursorSelectNextOnSameLevel id id

forestCursorSelectPrevOnSameLevel :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectPrevOnSameLevel = FC.forestCursorSelectPrevOnSameLevel id id

forestCursorSelectFirst :: ForestCursor a -> ForestCursor a
forestCursorSelectFirst = FC.forestCursorSelectFirst id id

forestCursorSelectLast :: ForestCursor a -> ForestCursor a
forestCursorSelectLast = FC.forestCursorSelectLast id id

forestCursorSelectBelowAtPos :: Int -> ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectBelowAtPos = FC.forestCursorSelectBelowAtPos id id

forestCursorSelectBelowAtStart :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectBelowAtStart = FC.forestCursorSelectBelowAtStart id id

forestCursorSelectBelowAtEnd :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectBelowAtEnd = FC.forestCursorSelectBelowAtEnd id id

forestCursorSelectIndex :: Int -> ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectIndex = FC.forestCursorSelectIndex id id

forestCursorInsertAndSelectTreeCursor ::
       TreeCursor a -> ForestCursor a -> ForestCursor a
forestCursorInsertAndSelectTreeCursor =
    FC.forestCursorInsertAndSelectTreeCursor id

forestCursorAppendAndSelectTreeCursor ::
       TreeCursor a -> ForestCursor a -> ForestCursor a
forestCursorAppendAndSelectTreeCursor =
    FC.forestCursorAppendAndSelectTreeCursor id

forestCursorInsertAndSelectTree :: Tree a -> ForestCursor a -> ForestCursor a
forestCursorInsertAndSelectTree = FC.forestCursorInsertAndSelectTree id id

forestCursorAppendAndSelectTree :: Tree a -> ForestCursor a -> ForestCursor a
forestCursorAppendAndSelectTree = FC.forestCursorAppendAndSelectTree id id

forestCursorInsertAndSelect :: a -> ForestCursor a -> ForestCursor a
forestCursorInsertAndSelect = FC.forestCursorInsertAndSelect id id

forestCursorAppendAndSelect :: a -> ForestCursor a -> ForestCursor a
forestCursorAppendAndSelect = FC.forestCursorAppendAndSelect id id

forestCursorRemoveElemAndSelectPrev ::
       ForestCursor a -> Maybe (DeleteOrUpdate (ForestCursor a))
forestCursorRemoveElemAndSelectPrev = FC.forestCursorRemoveElemAndSelectPrev id

forestCursorDeleteElemAndSelectNext ::
       ForestCursor a -> Maybe (DeleteOrUpdate (ForestCursor a))
forestCursorDeleteElemAndSelectNext = FC.forestCursorDeleteElemAndSelectNext id

forestCursorRemoveElem :: ForestCursor a -> DeleteOrUpdate (ForestCursor a)
forestCursorRemoveElem = FC.forestCursorRemoveElem id

forestCursorDeleteElem :: ForestCursor a -> DeleteOrUpdate (ForestCursor a)
forestCursorDeleteElem = FC.forestCursorDeleteElem id

forestCursorRemoveSubTreeAndSelectPrev ::
       ForestCursor a -> Maybe (DeleteOrUpdate (ForestCursor a))
forestCursorRemoveSubTreeAndSelectPrev =
    FC.forestCursorRemoveSubTreeAndSelectPrev id

forestCursorDeleteSubTreeAndSelectNext ::
       ForestCursor a -> Maybe (DeleteOrUpdate (ForestCursor a))
forestCursorDeleteSubTreeAndSelectNext =
    FC.forestCursorDeleteSubTreeAndSelectNext id

forestCursorRemoveSubTree :: ForestCursor a -> DeleteOrUpdate (ForestCursor a)
forestCursorRemoveSubTree = FC.forestCursorRemoveSubTree id

forestCursorDeleteSubTree :: ForestCursor a -> DeleteOrUpdate (ForestCursor a)
forestCursorDeleteSubTree = FC.forestCursorDeleteSubTree id

forestCursorAddRoot :: ForestCursor a -> a -> TreeCursor a
forestCursorAddRoot = FC.forestCursorAddRoot id id

forestCursorPromoteElem :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorPromoteElem = FC.forestCursorPromoteElem id id

forestCursorPromoteSubTree :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorPromoteSubTree = FC.forestCursorPromoteSubTree id id

forestCursorDemoteElem :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorDemoteElem = FC.forestCursorDemoteElem id id

forestCursorDemoteSubTree :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorDemoteSubTree = FC.forestCursorDemoteSubTree id id
