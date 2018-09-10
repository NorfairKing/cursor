{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.Forest
    ( ForestCursor(..)
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

import Cursor.List.NonEmpty
import Cursor.Tree
import Cursor.Types

newtype ForestCursor a b = ForestCursor
    { forestCursorListCursor :: NonEmptyCursor (TreeCursor a b) (Tree b)
    } deriving (Show, Eq, Generic)

instance (Validity a, Validity b) => Validity (ForestCursor a b)

makeForestCursor :: (b -> a) -> NonEmpty (Tree b) -> ForestCursor a b
makeForestCursor g = ForestCursor . makeNonEmptyCursor (makeTreeCursor g)

rebuildForestCursor :: (a -> b) -> ForestCursor a b -> NonEmpty (Tree b)
rebuildForestCursor f =
    rebuildNonEmptyCursor (rebuildTreeCursor f) . forestCursorListCursor

drawForestCursor :: (Show a, Show b) => ForestCursor a b -> String
drawForestCursor ForestCursor {..} =
    drawForest $
    (map (fmap show) $ reverse $ nonEmptyCursorPrev forestCursorListCursor) ++
    [treeCursorWithPointer $ nonEmptyCursorCurrent forestCursorListCursor] ++
    (map (fmap show) $ nonEmptyCursorNext forestCursorListCursor)

mapForestCursor :: (a -> c) -> (b -> d) -> ForestCursor a b -> ForestCursor c d
mapForestCursor f g =
    forestCursorListCursorL %~ mapNonEmptyCursor (mapTreeCursor f g) (fmap g)

forestCursorListCursorL ::
       Lens (ForestCursor a b) (ForestCursor c d) (NonEmptyCursor (TreeCursor a b) (Tree b)) (NonEmptyCursor (TreeCursor c d) (Tree d))
forestCursorListCursorL =
    lens forestCursorListCursor $ \fc lc -> fc {forestCursorListCursor = lc}

forestCursorSelectedTreeL :: Lens' (ForestCursor a b) (TreeCursor a b)
forestCursorSelectedTreeL = forestCursorListCursorL . nonEmptyCursorElemL

forestCursorSelectPrevTreeCursor ::
       (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectPrevTreeCursor f g =
    forestCursorListCursorL $
    nonEmptyCursorSelectPrev (rebuildTreeCursor f) (makeTreeCursor g)

forestCursorSelectNextTreeCursor ::
       (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectNextTreeCursor f g =
    forestCursorListCursorL $
    nonEmptyCursorSelectNext (rebuildTreeCursor f) (makeTreeCursor g)

forestCursorSelectFirstTreeCursor ::
       (a -> b) -> (b -> a) -> ForestCursor a b -> ForestCursor a b
forestCursorSelectFirstTreeCursor f g =
    forestCursorListCursorL %~
    (nonEmptyCursorSelectFirst (rebuildTreeCursor f) (makeTreeCursor g))

forestCursorSelectLastTreeCursor ::
       (a -> b) -> (b -> a) -> ForestCursor a b -> ForestCursor a b
forestCursorSelectLastTreeCursor f g =
    forestCursorListCursorL %~
    (nonEmptyCursorSelectLast (rebuildTreeCursor f) (makeTreeCursor g))

forestCursorSelectNext ::
       (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectNext f g fc =
    (fc & forestCursorSelectedTreeL (treeCursorSelectNext f g)) <|>
    forestCursorSelectNextTreeCursor f g fc

forestCursorSelectPrev ::
       (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectPrev f g fc =
    (fc & forestCursorSelectedTreeL (treeCursorSelectPrev f g)) <|>
    (forestCursorSelectPrevTreeCursor f g fc >>=
     forestCursorSelectedTreeL (treeCursorSelectBelowAtEndRecursively f g)) <|>
    (forestCursorSelectPrevTreeCursor f g fc)

forestCursorSelectNextOnSameLevel ::
       (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectNextOnSameLevel f g fc =
    (fc & forestCursorSelectedTreeL (treeCursorSelectNextOnSameLevel f g)) <|>
    forestCursorSelectNextTreeCursor f g fc

forestCursorSelectPrevOnSameLevel ::
       (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectPrevOnSameLevel f g fc =
    (fc & forestCursorSelectedTreeL (treeCursorSelectPrevOnSameLevel f g)) <|>
    forestCursorSelectPrevTreeCursor f g fc

forestCursorSelectBelowAtPos ::
       (a -> b)
    -> (b -> a)
    -> Int
    -> ForestCursor a b
    -> Maybe (ForestCursor a b)
forestCursorSelectBelowAtPos f g i =
    forestCursorSelectedTreeL $ treeCursorSelectBelowAtPos f g i

forestCursorSelectBelowAtStart ::
       (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectBelowAtStart f g =
    forestCursorSelectedTreeL $ treeCursorSelectBelowAtStart f g

forestCursorSelectBelowAtEnd ::
       (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectBelowAtEnd f g =
    forestCursorSelectedTreeL $ treeCursorSelectBelowAtEnd f g

forestCursorSelection :: ForestCursor a b -> Int
forestCursorSelection fc =
    nonEmptyCursorSelection $ fc ^. forestCursorListCursorL

forestCursorSelectIndex ::
       (a -> b)
    -> (b -> a)
    -> Int
    -> ForestCursor a b
    -> Maybe (ForestCursor a b)
forestCursorSelectIndex f g i =
    forestCursorListCursorL
        (nonEmptyCursorSelectIndex (rebuildTreeCursor f) (makeTreeCursor g) i)

forestCursorInsertEntireTree :: Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorInsertEntireTree t =
    forestCursorListCursorL %~ nonEmptyCursorInsert t

forestCursorInsertAndSelectTreeCursor ::
       (a -> b) -> TreeCursor a b -> ForestCursor a b -> ForestCursor a b
forestCursorInsertAndSelectTreeCursor f tc =
    forestCursorListCursorL %~
    nonEmptyCursorInsertAndSelect (rebuildTreeCursor f) tc

forestCursorAppendEntireTree :: Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAppendEntireTree t =
    forestCursorListCursorL %~ nonEmptyCursorAppend t

forestCursorAppendAndSelectTreeCursor ::
       (a -> b) -> TreeCursor a b -> ForestCursor a b -> ForestCursor a b
forestCursorAppendAndSelectTreeCursor f tc =
    forestCursorListCursorL %~
    nonEmptyCursorAppendAndSelect (rebuildTreeCursor f) tc

forestCursorInsertTree :: Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorInsertTree t fc =
    fromMaybe (forestCursorInsertEntireTree t fc) $
    fc & forestCursorSelectedTreeL (treeCursorInsert t)

forestCursorInsertAndSelectTree ::
       (a -> b) -> (b -> a) -> Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorInsertAndSelectTree f g t fc =
    fromMaybe (forestCursorInsertAndSelectTreeCursor f (makeTreeCursor g t) fc) $
    fc & forestCursorSelectedTreeL (treeCursorInsertAndSelect f g t)

forestCursorAppendTree :: Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAppendTree t fc =
    fromMaybe (forestCursorAppendEntireTree t fc) $
    fc & forestCursorSelectedTreeL (treeCursorAppend t)

forestCursorAppendAndSelectTree ::
       (a -> b) -> (b -> a) -> Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAppendAndSelectTree f g t fc =
    fromMaybe (forestCursorAppendAndSelectTreeCursor f (makeTreeCursor g t) fc) $
    fc & forestCursorSelectedTreeL (treeCursorAppendAndSelect f g t)

forestCursorInsert :: b -> ForestCursor a b -> ForestCursor a b
forestCursorInsert b = forestCursorInsertTree $ Node b []

forestCursorInsertAndSelect ::
       (a -> b) -> (b -> a) -> b -> ForestCursor a b -> ForestCursor a b
forestCursorInsertAndSelect f g b =
    forestCursorInsertAndSelectTree f g $ Node b []

forestCursorAppend :: b -> ForestCursor a b -> ForestCursor a b
forestCursorAppend b = forestCursorAppendTree $ Node b []

forestCursorAppendAndSelect ::
       (a -> b) -> (b -> a) -> b -> ForestCursor a b -> ForestCursor a b
forestCursorAppendAndSelect f g b =
    forestCursorAppendAndSelectTree f g $ Node b []

forestCursorAddChildTreeToNodeAtPos ::
       Int -> Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildTreeToNodeAtPos i t =
    forestCursorSelectedTreeL %~ treeCursorAddChildAtPos i t

forestCursorAddChildTreeToNodeAtStart ::
       Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildTreeToNodeAtStart t =
    forestCursorSelectedTreeL %~ treeCursorAddChildAtStart t

forestCursorAddChildTreeToNodeAtEnd ::
       Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildTreeToNodeAtEnd t fc =
    fc & forestCursorSelectedTreeL %~ treeCursorAddChildAtEnd t

forestCursorAddChildToNodeAtPos ::
       Int -> b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildToNodeAtPos i b =
    forestCursorAddChildTreeToNodeAtPos i $ Node b []

forestCursorAddChildToNodeAtStart :: b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildToNodeAtStart b =
    forestCursorAddChildTreeToNodeAtStart $ Node b []

forestCursorAddChildToNodeAtEnd :: b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildToNodeAtEnd b =
    forestCursorAddChildTreeToNodeAtEnd $ Node b []

forestCursorRemoveElemAndSelectPrev ::
       (b -> a) -> ForestCursor a b -> Maybe (DeleteOrUpdate (ForestCursor a b))
forestCursorRemoveElemAndSelectPrev g fc =
    joinPossibleDeletes
        (fc &
         focusPossibleDeleteOrUpdate
             forestCursorSelectedTreeL
             (treeCursorDeleteElemAndSelectPrevious g))
        (fc &
         focusPossibleDeleteOrUpdate
             forestCursorListCursorL
             (nonEmptyCursorRemoveElemAndSelectPrev (makeTreeCursor g)))

forestCursorDeleteElemAndSelectNext ::
       (b -> a) -> ForestCursor a b -> Maybe (DeleteOrUpdate (ForestCursor a b))
forestCursorDeleteElemAndSelectNext g fc =
    joinPossibleDeletes
        (fc &
         focusPossibleDeleteOrUpdate
             forestCursorSelectedTreeL
             (treeCursorDeleteElemAndSelectNext g))
        (fc &
         focusPossibleDeleteOrUpdate
             forestCursorListCursorL
             (nonEmptyCursorDeleteElemAndSelectNext (makeTreeCursor g)))

forestCursorRemoveElem ::
       (b -> a) -> ForestCursor a b -> DeleteOrUpdate (ForestCursor a b)
forestCursorRemoveElem g fc =
    (fc & forestCursorSelectedTreeL (treeCursorRemoveElem g)) <|>
    (fc & forestCursorListCursorL (nonEmptyCursorRemoveElem (makeTreeCursor g)))

forestCursorDeleteElem ::
       (b -> a) -> ForestCursor a b -> DeleteOrUpdate (ForestCursor a b)
forestCursorDeleteElem g fc =
    (fc & forestCursorSelectedTreeL (treeCursorDeleteElem g)) <|>
    (fc & forestCursorListCursorL (nonEmptyCursorDeleteElem (makeTreeCursor g)))

forestCursorRemoveSubTreeAndSelectPrev ::
       (b -> a) -> ForestCursor a b -> Maybe (DeleteOrUpdate (ForestCursor a b))
forestCursorRemoveSubTreeAndSelectPrev g fc =
    joinPossibleDeletes
        (fc &
         focusPossibleDeleteOrUpdate
             forestCursorSelectedTreeL
             (treeCursorDeleteSubTreeAndSelectPrevious g))
        (fc &
         focusPossibleDeleteOrUpdate
             forestCursorListCursorL
             (nonEmptyCursorRemoveElemAndSelectPrev (makeTreeCursor g)))

forestCursorDeleteSubTreeAndSelectNext ::
       (b -> a) -> ForestCursor a b -> Maybe (DeleteOrUpdate (ForestCursor a b))
forestCursorDeleteSubTreeAndSelectNext g fc =
    joinPossibleDeletes
        (fc &
         focusPossibleDeleteOrUpdate
             forestCursorSelectedTreeL
             (treeCursorDeleteSubTreeAndSelectNext g))
        (fc &
         focusPossibleDeleteOrUpdate
             forestCursorListCursorL
             (nonEmptyCursorDeleteElemAndSelectNext (makeTreeCursor g)))

forestCursorRemoveSubTree ::
       (b -> a) -> ForestCursor a b -> DeleteOrUpdate (ForestCursor a b)
forestCursorRemoveSubTree g fc =
    (fc & forestCursorSelectedTreeL (treeCursorRemoveSubTree g)) <|>
    (fc & forestCursorListCursorL (nonEmptyCursorRemoveElem (makeTreeCursor g)))

forestCursorDeleteSubTree ::
       (b -> a) -> ForestCursor a b -> DeleteOrUpdate (ForestCursor a b)
forestCursorDeleteSubTree g fc =
    (fc & forestCursorSelectedTreeL (treeCursorDeleteSubTree g)) <|>
    (fc & forestCursorListCursorL (nonEmptyCursorDeleteElem (makeTreeCursor g)))

forestCursorAddRoot ::
       (a -> b) -> (b -> a) -> ForestCursor a b -> a -> TreeCursor a b
forestCursorAddRoot f g fc v =
    makeTreeCursor g $ Node (f v) $ NE.toList $ rebuildForestCursor f fc
