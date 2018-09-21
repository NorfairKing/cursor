{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.Collapsible.Tree
    ( Collapse(..)
    , makeCollapse
    , rebuildCollapse
    , collapseValueL
    , collapseShowL
    , TreeCursor
    , TreeAbove(..)
    , TC.singletonTreeCursor
    , TC.makeTreeCursor
    , TC.makeTreeCursorWithSelection
    , TC.rebuildTreeCursor
    , TC.drawTreeCursor
    , TC.mapTreeCursor
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
    , TC.treeCursorSelect
    , TC.treeCursorSelectPrev
    , TC.treeCursorSelectNext
    , TC.treeCursorSelectFirst
    , TC.treeCursorSelectLast
    , TC.treeCursorSelectAbove
    , TC.treeCursorSelectBelowAtPos
    , TC.treeCursorSelectBelowAtStart
    , TC.treeCursorSelectBelowAtEnd
    , TC.treeCursorSelectBelowAtStartRecursively
    , TC.treeCursorSelectBelowAtEndRecursively
    , TC.treeCursorSelectPrevOnSameLevel
    , TC.treeCursorSelectNextOnSameLevel
    , TC.treeCursorSelectAbovePrev
    , TC.treeCursorSelectAboveNext
    , TC.treeCursorInsert
    , TC.treeCursorInsertAndSelect
    , TC.treeCursorAppend
    , TC.treeCursorAppendAndSelect
    , TC.treeCursorAddChildAtPos
    , TC.treeCursorAddChildAtStart
    , TC.treeCursorAddChildAtEnd
    , TC.treeCursorDeleteSubTreeAndSelectPrevious
    , TC.treeCursorDeleteSubTreeAndSelectNext
    , TC.treeCursorDeleteSubTreeAndSelectAbove
    , TC.treeCursorRemoveSubTree
    , TC.treeCursorDeleteSubTree
    , TC.treeCursorDeleteElemAndSelectPrevious
    , TC.treeCursorDeleteElemAndSelectNext
    , TC.treeCursorDeleteElemAndSelectAbove
    , TC.treeCursorRemoveElem
    , TC.treeCursorDeleteElem
    , TC.treeCursorSwapPrev
    , TC.treeCursorSwapNext
    , TC.SwapResult(..)
    , TC.treeCursorPromoteElem
    , TC.PromoteElemResult(..)
    , TC.treeCursorPromoteSubTree
    , TC.PromoteResult(..)
    , TC.treeCursorDemoteElem
    , TC.treeCursorDemoteSubTree
    , TC.DemoteResult(..)
    , TC.treeCursorDemoteElemUnder
    , TC.treeCursorDemoteSubTreeUnder
    ) where

import GHC.Generics (Generic)

import Data.Tree

import Lens.Micro

import Cursor.Types

import qualified Cursor.Tree as TC
import Cursor.Tree (TreeAbove(..))

type TreeCursor a b = TC.TreeCursor (Collapse a) (Collapse b)

data Collapse a = Collapse
    { collapseValue :: a
    , collapseShow :: Bool
    } deriving (Show, Eq, Generic)

makeCollapse :: a -> Collapse a
makeCollapse a = Collapse {collapseValue = a, collapseShow = True}

rebuildCollapse :: Collapse a -> a
rebuildCollapse = collapseValue

collapseValueL :: Lens (Collapse a) (Collapse b) a b
collapseValueL = lens collapseValue $ \c v -> c {collapseValue = v}

collapseShowL :: Lens' (Collapse a) Bool
collapseShowL = lens collapseShow $ \c b -> c {collapseShow = b}
