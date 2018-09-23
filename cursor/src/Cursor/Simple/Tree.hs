{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.Simple.Tree
    ( TreeCursor
    , TreeAbove(..)
    , singletonTreeCursor
    , makeTreeCursor
    , makeTreeCursorWithSelection
    , rebuildTreeCursor
    , TC.drawTreeCursor
    , mapTreeCursor
    , TC.treeCursorAboveL
    , TC.treeCursorCurrentL
    , TC.treeCursorBelowL
    , TC.treeAboveLeftsL
    , TC.treeAboveAboveL
    , TC.treeAboveNodeL
    , TC.treeAboveRightsL
    , TC.treeCursorWithPointer
    , TC.treeCursorSelection
    , TC.TreeCursorSelection(..)
    , treeCursorSelect
    , treeCursorSelectPrev
    , treeCursorSelectNext
    , treeCursorSelectFirst
    , treeCursorSelectLast
    , treeCursorSelectAbove
    , treeCursorSelectBelowAtPos
    , treeCursorSelectBelowAtStart
    , treeCursorSelectBelowAtEnd
    , treeCursorSelectBelowAtStartRecursively
    , treeCursorSelectBelowAtEndRecursively
    , treeCursorSelectPrevOnSameLevel
    , treeCursorSelectNextOnSameLevel
    , treeCursorSelectAbovePrev
    , treeCursorSelectAboveNext
    , TC.treeCursorInsert
    , treeCursorInsertAndSelect
    , TC.treeCursorAppend
    , treeCursorAppendAndSelect
    , TC.treeCursorAddChildAtPos
    , TC.treeCursorAddChildAtStart
    , TC.treeCursorAddChildAtEnd
    , treeCursorDeleteSubTreeAndSelectPrevious
    , treeCursorDeleteSubTreeAndSelectNext
    , treeCursorDeleteSubTreeAndSelectAbove
    , treeCursorRemoveSubTree
    , treeCursorDeleteSubTree
    , treeCursorDeleteElemAndSelectPrevious
    , treeCursorDeleteElemAndSelectNext
    , treeCursorDeleteElemAndSelectAbove
    , treeCursorRemoveElem
    , treeCursorDeleteElem
    , TC.treeCursorSwapPrev
    , TC.treeCursorSwapNext
    , TC.SwapResult(..)
    , treeCursorPromoteElem
    , TC.PromoteElemResult(..)
    , treeCursorPromoteSubTree
    , TC.PromoteResult(..)
    , treeCursorDemoteElem
    , treeCursorDemoteSubTree
    , TC.DemoteResult(..)
    , TC.treeCursorDemoteElemUnder
    , TC.treeCursorDemoteSubTreeUnder
    , TC.CTree(..)
    , TC.CForest
    , TC.makeCTree
    , TC.cTree
    , TC.rebuildCTree
    ) where

import Data.Tree

import Lens.Micro

import Cursor.Types

import qualified Cursor.Tree as TC
import Cursor.Tree (CForest, CTree, TreeAbove(..))

type TreeCursor a = TC.TreeCursor a a

makeTreeCursor :: CTree a -> TreeCursor a
makeTreeCursor = TC.makeTreeCursor id

makeTreeCursorWithSelection ::
       TC.TreeCursorSelection -> CTree a -> Maybe (TreeCursor a)
makeTreeCursorWithSelection = TC.makeTreeCursorWithSelection id id

singletonTreeCursor :: a -> TreeCursor a
singletonTreeCursor = TC.singletonTreeCursor

rebuildTreeCursor :: TreeCursor a -> CTree a
rebuildTreeCursor = TC.rebuildTreeCursor id

mapTreeCursor :: (a -> b) -> TreeCursor a -> TreeCursor b
mapTreeCursor f = TC.mapTreeCursor f f

treeCursorSelect ::
       TC.TreeCursorSelection -> TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelect = TC.treeCursorSelect id id

treeCursorSelectPrev :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectPrev = TC.treeCursorSelectPrev id id

treeCursorSelectNext :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectNext = TC.treeCursorSelectNext id id

treeCursorSelectFirst :: TreeCursor a -> TreeCursor a
treeCursorSelectFirst = TC.treeCursorSelectFirst id id

treeCursorSelectLast :: TreeCursor a -> TreeCursor a
treeCursorSelectLast = TC.treeCursorSelectLast id id

treeCursorSelectAbove :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectAbove = TC.treeCursorSelectAbove id id

treeCursorSelectBelowAtPos :: Int -> TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectBelowAtPos = TC.treeCursorSelectBelowAtPos id id

treeCursorSelectBelowAtStart :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectBelowAtStart = TC.treeCursorSelectBelowAtStart id id

treeCursorSelectBelowAtEnd :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectBelowAtEnd = TC.treeCursorSelectBelowAtEnd id id

treeCursorSelectBelowAtStartRecursively :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectBelowAtStartRecursively =
    TC.treeCursorSelectBelowAtStartRecursively id id

treeCursorSelectBelowAtEndRecursively :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectBelowAtEndRecursively =
    TC.treeCursorSelectBelowAtEndRecursively id id

treeCursorSelectPrevOnSameLevel :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectPrevOnSameLevel = TC.treeCursorSelectPrevOnSameLevel id id

treeCursorSelectNextOnSameLevel :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectNextOnSameLevel = TC.treeCursorSelectNextOnSameLevel id id

-- | Go back and down as far as necessary to find a previous element on a level below
treeCursorSelectAbovePrev :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectAbovePrev = TC.treeCursorSelectAbovePrev id id

-- | Go up as far as necessary to find a next element on a level above and forward
--
-- Note: This will fail if there is a next node on the same level or any node below the current node
treeCursorSelectAboveNext :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectAboveNext = TC.treeCursorSelectAboveNext id id

treeCursorInsertAndSelect :: Tree a -> TreeCursor a -> Maybe (TreeCursor a)
treeCursorInsertAndSelect = TC.treeCursorInsertAndSelect id id

treeCursorAppendAndSelect :: Tree a -> TreeCursor a -> Maybe (TreeCursor a)
treeCursorAppendAndSelect = TC.treeCursorAppendAndSelect id id

treeCursorDeleteSubTreeAndSelectPrevious ::
       TreeCursor a -> Maybe (DeleteOrUpdate (TreeCursor a))
treeCursorDeleteSubTreeAndSelectPrevious =
    TC.treeCursorDeleteSubTreeAndSelectPrevious id

treeCursorDeleteSubTreeAndSelectNext ::
       TreeCursor a -> Maybe (DeleteOrUpdate (TreeCursor a))
treeCursorDeleteSubTreeAndSelectNext =
    TC.treeCursorDeleteSubTreeAndSelectNext id

treeCursorDeleteSubTreeAndSelectAbove ::
       TreeCursor a -> DeleteOrUpdate (TreeCursor a)
treeCursorDeleteSubTreeAndSelectAbove =
    TC.treeCursorDeleteSubTreeAndSelectAbove id

treeCursorRemoveSubTree :: TreeCursor a -> DeleteOrUpdate (TreeCursor a)
treeCursorRemoveSubTree = TC.treeCursorRemoveSubTree id

treeCursorDeleteSubTree :: TreeCursor a -> DeleteOrUpdate (TreeCursor a)
treeCursorDeleteSubTree = TC.treeCursorDeleteSubTree id

treeCursorDeleteElemAndSelectPrevious ::
       TreeCursor a -> Maybe (DeleteOrUpdate (TreeCursor a))
treeCursorDeleteElemAndSelectPrevious =
    TC.treeCursorDeleteElemAndSelectPrevious id

treeCursorDeleteElemAndSelectNext ::
       TreeCursor a -> Maybe (DeleteOrUpdate (TreeCursor a))
treeCursorDeleteElemAndSelectNext = TC.treeCursorDeleteElemAndSelectNext id

treeCursorDeleteElemAndSelectAbove ::
       TreeCursor a -> Maybe (DeleteOrUpdate (TreeCursor a))
treeCursorDeleteElemAndSelectAbove = TC.treeCursorDeleteElemAndSelectAbove id

treeCursorRemoveElem :: TreeCursor a -> DeleteOrUpdate (TreeCursor a)
treeCursorRemoveElem = TC.treeCursorRemoveElem id

treeCursorDeleteElem :: TreeCursor a -> DeleteOrUpdate (TreeCursor a)
treeCursorDeleteElem = TC.treeCursorDeleteElem id

treeCursorPromoteElem :: TreeCursor a -> TC.PromoteElemResult (TreeCursor a)
treeCursorPromoteElem = TC.treeCursorPromoteElem id id

treeCursorPromoteSubTree :: TreeCursor a -> TC.PromoteResult (TreeCursor a)
treeCursorPromoteSubTree = TC.treeCursorPromoteSubTree id id

treeCursorDemoteElem :: TreeCursor a -> TC.DemoteResult (TreeCursor a)
treeCursorDemoteElem = TC.treeCursorDemoteElem id id

treeCursorDemoteSubTree :: TreeCursor a -> TC.DemoteResult (TreeCursor a)
treeCursorDemoteSubTree = TC.treeCursorDemoteSubTree id id
