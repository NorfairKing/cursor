{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.Simple.Tree
    ( TreeCursor
    , TreeAbove(..)
    , singletonTreeCursor
    , makeTreeCursor
    , rebuildTreeCursor
    , drawTreeCursor
    , mapTreeCursor
    , treeCursorAboveL
    , treeCursorCurrentL
    , treeCursorBelowL
    , treeAboveLeftsL
    , treeAboveAboveL
    , treeAboveNodeL
    , treeAboveRightsL
    , treeCursorWithPointer
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
    , treeCursorInsert
    , treeCursorInsertAndSelect
    , treeCursorAppend
    , treeCursorAppendAndSelect
    , treeCursorAddChildAtPos
    , treeCursorAddChildAtStart
    , treeCursorAddChildAtEnd
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
    , treeCursorSwapPrev
    , treeCursorSwapNext
    , treeCursorPromoteElem
    , treeCursorDemoteElem
    , treeCursorPromoteSubTree
    , treeCursorDemoteSubTree
    ) where

import Data.Tree

import Lens.Micro

import Cursor.Types

import qualified Cursor.Tree as TC
import Cursor.Tree (TreeAbove(..))

type TreeCursor a = TC.TreeCursor a a

treeCursorAboveL :: Lens' (TreeCursor a) (Maybe (TreeAbove a))
treeCursorAboveL = TC.treeCursorAboveL

treeCursorCurrentL :: Lens' (TreeCursor a) a
treeCursorCurrentL = TC.treeCursorCurrentL

treeCursorBelowL :: Lens' (TreeCursor a) (Forest a)
treeCursorBelowL = TC.treeCursorBelowL

treeAboveLeftsL :: Lens' (TreeAbove a) [Tree a]
treeAboveLeftsL = lens treeAboveLefts $ \ta tal -> ta {treeAboveLefts = tal}

treeAboveAboveL :: Lens' (TreeAbove a) (Maybe (TreeAbove a))
treeAboveAboveL = lens treeAboveAbove $ \ta taa -> ta {treeAboveAbove = taa}

treeAboveNodeL :: Lens' (TreeAbove a) a
treeAboveNodeL = lens treeAboveNode $ \ta a -> ta {treeAboveNode = a}

treeAboveRightsL :: Lens' (TreeAbove a) [Tree a]
treeAboveRightsL = lens treeAboveRights $ \ta tar -> ta {treeAboveRights = tar}

makeTreeCursor :: Tree a -> TreeCursor a
makeTreeCursor = TC.makeTreeCursor id

singletonTreeCursor :: a -> TreeCursor a
singletonTreeCursor = TC.singletonTreeCursor

rebuildTreeCursor :: TreeCursor a -> Tree a
rebuildTreeCursor = TC.rebuildTreeCursor id

drawTreeCursor :: Show a => TreeCursor a -> String
drawTreeCursor = TC.drawTreeCursor

mapTreeCursor :: (a -> b) -> TreeCursor a -> TreeCursor b
mapTreeCursor f = TC.mapTreeCursor f f

treeCursorWithPointer :: Show a => TreeCursor a -> Tree String
treeCursorWithPointer = TC.treeCursorWithPointer

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

treeCursorInsert :: Tree a -> TreeCursor a -> Maybe (TreeCursor a)
treeCursorInsert = TC.treeCursorInsert

treeCursorInsertAndSelect :: Tree a -> TreeCursor a -> Maybe (TreeCursor a)
treeCursorInsertAndSelect = TC.treeCursorInsertAndSelect id id

treeCursorAppend :: Tree a -> TreeCursor a -> Maybe (TreeCursor a)
treeCursorAppend = TC.treeCursorAppend

treeCursorAppendAndSelect :: Tree a -> TreeCursor a -> Maybe (TreeCursor a)
treeCursorAppendAndSelect = TC.treeCursorAppendAndSelect id id

treeCursorAddChildAtPos :: Int -> Tree a -> TreeCursor a -> TreeCursor a
treeCursorAddChildAtPos = TC.treeCursorAddChildAtPos

treeCursorAddChildAtStart :: Tree a -> TreeCursor a -> TreeCursor a
treeCursorAddChildAtStart = TC.treeCursorAddChildAtStart

treeCursorAddChildAtEnd :: Tree a -> TreeCursor a -> TreeCursor a
treeCursorAddChildAtEnd = TC.treeCursorAddChildAtEnd

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

treeCursorSwapPrev :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSwapPrev = TC.treeCursorSwapPrev id id

treeCursorSwapNext :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSwapNext = TC.treeCursorSwapNext id id

treeCursorPromoteSubTree :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorPromoteSubTree = TC.treeCursorPromoteSubTree id id

treeCursorDemoteSubTree :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorDemoteSubTree = TC.treeCursorDemoteSubTree id id

treeCursorPromoteElem :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorPromoteElem = TC.treeCursorPromoteElem id id

treeCursorDemoteElem :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorDemoteElem = TC.treeCursorDemoteElem id id
