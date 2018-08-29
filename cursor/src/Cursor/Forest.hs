{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.Forest
    ( ForestCursor(..)
    , makeForestCursor
    , rebuildForestCursor
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
    , forestCursorInsertTreeCursor
    , forestCursorAppendTreeCursor
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
    , forestCursorRemoveTreeAndSelectPrev
    , forestCursorDeleteTreeAndSelectNext
    , forestCursorRemoveTree
    , forestCursorDeleteTree
    , forestCursorAddRoot
    ) where

import GHC.Generics (Generic)

import Data.Validity
import Data.Validity.Tree ()

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.Tree

import Control.Applicative

import Lens.Micro

import Cursor.NonEmpty
import Cursor.Tree
import Cursor.Types

newtype ForestCursor a = ForestCursor
    { forestCursorListCursor :: NonEmptyCursor (TreeCursor a)
    } deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (ForestCursor a)

makeForestCursor :: NonEmpty (Tree a) -> ForestCursor a
makeForestCursor = ForestCursor . makeNonEmptyCursor . NE.map makeTreeCursor

rebuildForestCursor :: ForestCursor a -> NonEmpty (Tree a)
rebuildForestCursor =
    NE.map rebuildTreeCursor . rebuildNonEmptyCursor . forestCursorListCursor

forestCursorListCursorL ::
       Lens' (ForestCursor a) (NonEmptyCursor (TreeCursor a))
forestCursorListCursorL =
    lens forestCursorListCursor $ \fc lc -> fc {forestCursorListCursor = lc}

forestCursorSelectedTreeL :: Lens' (ForestCursor a) (TreeCursor a)
forestCursorSelectedTreeL = forestCursorListCursorL . nonEmptyCursorElemL

forestCursorSelectPrevTreeCursor :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectPrevTreeCursor =
    forestCursorListCursorL nonEmptyCursorSelectPrev

forestCursorSelectNextTreeCursor :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectNextTreeCursor =
    forestCursorListCursorL nonEmptyCursorSelectNext

forestCursorSelectFirstTreeCursor :: ForestCursor a -> ForestCursor a
forestCursorSelectFirstTreeCursor =
    forestCursorListCursorL %~ nonEmptyCursorSelectFirst

forestCursorSelectLastTreeCursor :: ForestCursor a -> ForestCursor a
forestCursorSelectLastTreeCursor =
    forestCursorListCursorL %~ nonEmptyCursorSelectLast

forestCursorSelectNext :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectNext fc =
    (fc & forestCursorSelectedTreeL treeCursorSelectNext) <|>
    forestCursorSelectNextTreeCursor fc

forestCursorSelectPrev :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectPrev fc =
    (fc & forestCursorSelectedTreeL treeCursorSelectPrev) <|>
    forestCursorSelectPrevTreeCursor fc

forestCursorSelectNextOnSameLevel :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectNextOnSameLevel fc =
    (fc & forestCursorSelectedTreeL treeCursorSelectNextOnSameLevel) <|>
    forestCursorSelectNextTreeCursor fc

forestCursorSelectPrevOnSameLevel :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectPrevOnSameLevel fc =
    (fc & forestCursorSelectedTreeL treeCursorSelectPrevOnSameLevel) <|>
    forestCursorSelectPrevTreeCursor fc

forestCursorSelectBelowAtPos :: Int -> ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectBelowAtPos i =
    forestCursorSelectedTreeL $ treeCursorSelectBelowAtPos i

forestCursorSelectBelowAtStart :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectBelowAtStart =
    forestCursorSelectedTreeL treeCursorSelectBelowAtStart

forestCursorSelectBelowAtEnd :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectBelowAtEnd =
    forestCursorSelectedTreeL treeCursorSelectBelowAtEnd

forestCursorSelection :: ForestCursor a -> Int
forestCursorSelection fc =
    nonEmptyCursorSelection $ fc ^. forestCursorListCursorL

forestCursorSelectIndex :: ForestCursor a -> Int -> Maybe (ForestCursor a)
forestCursorSelectIndex fc i =
    fc & forestCursorListCursorL (`nonEmptyCursorSelectIndex` i)

forestCursorInsertTreeCursor :: TreeCursor a -> ForestCursor a -> ForestCursor a
forestCursorInsertTreeCursor tc fc =
    fc & forestCursorListCursorL %~ nonEmptyCursorInsert tc

forestCursorInsertAndSelectTreeCursor ::
       TreeCursor a -> ForestCursor a -> ForestCursor a
forestCursorInsertAndSelectTreeCursor tc fc =
    fc & forestCursorListCursorL %~ nonEmptyCursorInsertAndSelect tc

forestCursorAppendTreeCursor :: TreeCursor a -> ForestCursor a -> ForestCursor a
forestCursorAppendTreeCursor tc fc =
    fc & forestCursorListCursorL %~ nonEmptyCursorAppend tc

forestCursorAppendAndSelectTreeCursor ::
       TreeCursor a -> ForestCursor a -> ForestCursor a
forestCursorAppendAndSelectTreeCursor tc fc =
    fc & forestCursorListCursorL %~ nonEmptyCursorAppendAndSelect tc

forestCursorInsertTree :: Tree a -> ForestCursor a -> ForestCursor a
forestCursorInsertTree t fc =
    fromMaybe (forestCursorInsertTreeCursor (makeTreeCursor t) fc) $
    fc & forestCursorSelectedTreeL (treeCursorInsert t)

forestCursorInsertAndSelectTree :: Tree a -> ForestCursor a -> ForestCursor a
forestCursorInsertAndSelectTree t fc =
    fromMaybe (forestCursorInsertAndSelectTreeCursor (makeTreeCursor t) fc) $
    fc & forestCursorSelectedTreeL (treeCursorInsertAndSelect t)

forestCursorAppendTree :: Tree a -> ForestCursor a -> ForestCursor a
forestCursorAppendTree t fc =
    fromMaybe (forestCursorAppendTreeCursor (makeTreeCursor t) fc) $
    fc & forestCursorSelectedTreeL (treeCursorAppend t)

forestCursorAppendAndSelectTree :: Tree a -> ForestCursor a -> ForestCursor a
forestCursorAppendAndSelectTree t fc =
    fromMaybe (forestCursorAppendAndSelectTreeCursor (makeTreeCursor t) fc) $
    fc & forestCursorSelectedTreeL (treeCursorAppendAndSelect t)

forestCursorInsert :: a -> ForestCursor a -> ForestCursor a
forestCursorInsert a = forestCursorInsertTree $ Node a []

forestCursorInsertAndSelect :: a -> ForestCursor a -> ForestCursor a
forestCursorInsertAndSelect a = forestCursorInsertAndSelectTree $ Node a []

forestCursorAppend :: a -> ForestCursor a -> ForestCursor a
forestCursorAppend a = forestCursorAppendTree $ Node a []

forestCursorAppendAndSelect :: a -> ForestCursor a -> ForestCursor a
forestCursorAppendAndSelect a = forestCursorAppendAndSelectTree $ Node a []

forestCursorAddChildTreeToNodeAtPos ::
       Int -> Tree a -> ForestCursor a -> ForestCursor a
forestCursorAddChildTreeToNodeAtPos i t fc =
    fc & forestCursorSelectedTreeL %~ treeCursorAddChildAtPos i t

forestCursorAddChildTreeToNodeAtStart ::
       Tree a -> ForestCursor a -> ForestCursor a
forestCursorAddChildTreeToNodeAtStart t fc =
    fc & forestCursorSelectedTreeL %~ treeCursorAddChildAtStart t

forestCursorAddChildTreeToNodeAtEnd ::
       Tree a -> ForestCursor a -> ForestCursor a
forestCursorAddChildTreeToNodeAtEnd t fc =
    fc & forestCursorSelectedTreeL %~ treeCursorAddChildAtEnd t

forestCursorAddChildToNodeAtPos :: Int -> a -> ForestCursor a -> ForestCursor a
forestCursorAddChildToNodeAtPos i a =
    forestCursorAddChildTreeToNodeAtPos i $ Node a []

forestCursorAddChildToNodeAtStart :: a -> ForestCursor a -> ForestCursor a
forestCursorAddChildToNodeAtStart a =
    forestCursorAddChildTreeToNodeAtStart $ Node a []

forestCursorAddChildToNodeAtEnd :: a -> ForestCursor a -> ForestCursor a
forestCursorAddChildToNodeAtEnd a =
    forestCursorAddChildTreeToNodeAtEnd $ Node a []

forestCursorRemoveTreeAndSelectPrev ::
       ForestCursor a -> Maybe (DeleteOrUpdate (ForestCursor a))
forestCursorRemoveTreeAndSelectPrev fc =
    joinPossibleDeletes
        (fc &
         focusPossibleDeleteOrUpdate
             forestCursorSelectedTreeL
             treeCursorDeleteElemAndSelectPrevious)
        (fc &
         focusPossibleDeleteOrUpdate
             forestCursorListCursorL
             nonEmptyCursorRemoveElemAndSelectPrev)

forestCursorDeleteTreeAndSelectNext ::
       ForestCursor a -> Maybe (DeleteOrUpdate (ForestCursor a))
forestCursorDeleteTreeAndSelectNext fc =
    joinPossibleDeletes
        (fc &
         focusPossibleDeleteOrUpdate
             forestCursorSelectedTreeL
             treeCursorDeleteElemAndSelectNext)
        (fc &
         focusPossibleDeleteOrUpdate
             forestCursorListCursorL
             nonEmptyCursorDeleteElemAndSelectNext)

forestCursorRemoveTree :: ForestCursor a -> DeleteOrUpdate (ForestCursor a)
forestCursorRemoveTree fc =
    (fc & forestCursorSelectedTreeL treeCursorRemoveElem) <|>
    (fc & forestCursorListCursorL nonEmptyCursorRemoveElem)

forestCursorDeleteTree :: ForestCursor a -> DeleteOrUpdate (ForestCursor a)
forestCursorDeleteTree fc =
    (fc & forestCursorSelectedTreeL treeCursorDeleteElem) <|>
    (fc & forestCursorListCursorL nonEmptyCursorDeleteElem)

forestCursorAddRoot :: ForestCursor a -> a -> TreeCursor a
forestCursorAddRoot fc v =
    makeTreeCursor $ Node v $ NE.toList $ rebuildForestCursor fc
