{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.Simple.Forest
    ( ForestCursor
    , makeForestCursor
    , rebuildForestCursor
    , drawForestCursor
    , mapForestCursor
    , forestCursorListCursorL
    , forestCursorSelectedTreeL
    , forestCursorSelectPrevTreeCursor
    , forestCursorSelectNextTreeCursor
    , forestCursorSelectFirstTreeCursor
    , forestCursorSelectLastTreeCursor
    , forestCursorSelectPrev
    , forestCursorSelectNext
    , forestCursorSelectPrevOnSameLevel
    , forestCursorSelectNextOnSameLevel
    , forestCursorSelectBelowAtPos
    , forestCursorSelectBelowAtStart
    , forestCursorSelectBelowAtEnd
    , forestCursorSelection
    , forestCursorSelectIndex
    , forestCursorInsertEntireTree
    , forestCursorAppendEntireTree
    , forestCursorInsertAndSelectTreeCursor
    , forestCursorAppendAndSelectTreeCursor
    , forestCursorInsertTree
    , forestCursorAppendTree
    , forestCursorInsertAndSelectTree
    , forestCursorAppendAndSelectTree
    , forestCursorInsert
    , forestCursorAppend
    , forestCursorInsertAndSelect
    , forestCursorAppendAndSelect
    , forestCursorAddChildTreeToNodeAtPos
    , forestCursorAddChildTreeToNodeAtStart
    , forestCursorAddChildTreeToNodeAtEnd
    , forestCursorAddChildToNodeAtPos
    , forestCursorAddChildToNodeAtStart
    , forestCursorAddChildToNodeAtEnd
    , forestCursorRemoveElemAndSelectPrev
    , forestCursorDeleteElemAndSelectNext
    , forestCursorRemoveElem
    , forestCursorDeleteElem
    , forestCursorRemoveSubTreeAndSelectPrev
    , forestCursorDeleteSubTreeAndSelectNext
    , forestCursorRemoveSubTree
    , forestCursorDeleteSubTree
    , forestCursorAddRoot
    , forestCursorSwapPrev
    , forestCursorSwapNext
    , forestCursorPromoteElem
    , forestCursorPromoteSubTree
    , forestCursorDemoteElem
    , forestCursorDemoteSubTree
    , forestCursorDemoteElemUnder
    , forestCursorDemoteSubTreeUnder
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

makeForestCursor :: NonEmpty (Tree a) -> ForestCursor a
makeForestCursor = FC.makeForestCursor id

rebuildForestCursor :: ForestCursor a -> NonEmpty (Tree a)
rebuildForestCursor = FC.rebuildForestCursor id

drawForestCursor :: Show a => ForestCursor a -> String
drawForestCursor = FC.drawForestCursor

mapForestCursor :: (a -> b) -> ForestCursor a -> ForestCursor b
mapForestCursor f = FC.mapForestCursor f f

forestCursorListCursorL ::
       Lens (ForestCursor a) (ForestCursor b) (NonEmptyCursor (TreeCursor a) (Tree a)) (NonEmptyCursor (TreeCursor b) (Tree b))
forestCursorListCursorL = FC.forestCursorListCursorL

forestCursorSelectedTreeL :: Lens' (ForestCursor a) (TreeCursor a)
forestCursorSelectedTreeL = FC.forestCursorSelectedTreeL

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

forestCursorSelectBelowAtPos :: Int -> ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectBelowAtPos = FC.forestCursorSelectBelowAtPos id id

forestCursorSelectBelowAtStart :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectBelowAtStart = FC.forestCursorSelectBelowAtStart id id

forestCursorSelectBelowAtEnd :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectBelowAtEnd = FC.forestCursorSelectBelowAtEnd id id

forestCursorSelection :: ForestCursor a -> Int
forestCursorSelection = FC.forestCursorSelection

forestCursorSelectIndex :: Int -> ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectIndex = FC.forestCursorSelectIndex id id

forestCursorInsertEntireTree :: Tree a -> ForestCursor a -> ForestCursor a
forestCursorInsertEntireTree = FC.forestCursorInsertEntireTree

forestCursorInsertAndSelectTreeCursor ::
       TreeCursor a -> ForestCursor a -> ForestCursor a
forestCursorInsertAndSelectTreeCursor =
    FC.forestCursorInsertAndSelectTreeCursor id

forestCursorAppendEntireTree :: Tree a -> ForestCursor a -> ForestCursor a
forestCursorAppendEntireTree = FC.forestCursorAppendEntireTree

forestCursorAppendAndSelectTreeCursor ::
       TreeCursor a -> ForestCursor a -> ForestCursor a
forestCursorAppendAndSelectTreeCursor =
    FC.forestCursorAppendAndSelectTreeCursor id

forestCursorInsertTree :: Tree a -> ForestCursor a -> ForestCursor a
forestCursorInsertTree = FC.forestCursorInsertTree

forestCursorInsertAndSelectTree :: Tree a -> ForestCursor a -> ForestCursor a
forestCursorInsertAndSelectTree = FC.forestCursorInsertAndSelectTree id id

forestCursorAppendTree :: Tree a -> ForestCursor a -> ForestCursor a
forestCursorAppendTree = FC.forestCursorAppendTree

forestCursorAppendAndSelectTree :: Tree a -> ForestCursor a -> ForestCursor a
forestCursorAppendAndSelectTree = FC.forestCursorAppendAndSelectTree id id

forestCursorInsert :: a -> ForestCursor a -> ForestCursor a
forestCursorInsert = FC.forestCursorInsert

forestCursorInsertAndSelect :: a -> ForestCursor a -> ForestCursor a
forestCursorInsertAndSelect = FC.forestCursorInsertAndSelect id id

forestCursorAppend :: a -> ForestCursor a -> ForestCursor a
forestCursorAppend = FC.forestCursorAppend

forestCursorAppendAndSelect :: a -> ForestCursor a -> ForestCursor a
forestCursorAppendAndSelect = FC.forestCursorAppendAndSelect id id

forestCursorAddChildTreeToNodeAtPos ::
       Int -> Tree a -> ForestCursor a -> ForestCursor a
forestCursorAddChildTreeToNodeAtPos = FC.forestCursorAddChildTreeToNodeAtPos

forestCursorAddChildTreeToNodeAtStart ::
       Tree a -> ForestCursor a -> ForestCursor a
forestCursorAddChildTreeToNodeAtStart = FC.forestCursorAddChildTreeToNodeAtStart

forestCursorAddChildTreeToNodeAtEnd ::
       Tree a -> ForestCursor a -> ForestCursor a
forestCursorAddChildTreeToNodeAtEnd = FC.forestCursorAddChildTreeToNodeAtEnd

forestCursorAddChildToNodeAtPos :: Int -> a -> ForestCursor a -> ForestCursor a
forestCursorAddChildToNodeAtPos = FC.forestCursorAddChildToNodeAtPos

forestCursorAddChildToNodeAtStart :: a -> ForestCursor a -> ForestCursor a
forestCursorAddChildToNodeAtStart = FC.forestCursorAddChildToNodeAtStart

forestCursorAddChildToNodeAtEnd :: a -> ForestCursor a -> ForestCursor a
forestCursorAddChildToNodeAtEnd = FC.forestCursorAddChildToNodeAtEnd

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

forestCursorSwapPrev :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorSwapPrev = FC.forestCursorSwapPrev

forestCursorSwapNext :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorSwapNext = FC.forestCursorSwapNext

forestCursorPromoteElem :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorPromoteElem = FC.forestCursorPromoteElem id id

forestCursorPromoteSubTree :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorPromoteSubTree = FC.forestCursorPromoteSubTree id id

forestCursorDemoteElem :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorDemoteElem = FC.forestCursorDemoteElem id id

forestCursorDemoteSubTree :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorDemoteSubTree = FC.forestCursorDemoteSubTree id id

forestCursorDemoteElemUnder :: a -> a -> ForestCursor a -> ForestCursor a
forestCursorDemoteElemUnder = FC.forestCursorDemoteElemUnder

forestCursorDemoteSubTreeUnder :: a -> ForestCursor a -> ForestCursor a
forestCursorDemoteSubTreeUnder = FC.forestCursorDemoteSubTreeUnder
