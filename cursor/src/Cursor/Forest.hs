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
    , forestCursorSelectPrevTree
    , forestCursorSelectNextTree
    , forestCursorSelectFirstTree
    , forestCursorSelectLastTree
    , forestCursorInsert
    , forestCursorAppend
    , forestCursorInsertAndSelect
    , forestCursorAppendAndSelect
    ) where

import GHC.Generics (Generic)

import Data.Validity
import Data.Validity.Tree ()

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import Data.Tree

import Lens.Micro

import Cursor.NonEmpty
import Cursor.Tree

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

forestCursorSelectPrevTree :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectPrevTree = forestCursorListCursorL nonEmptyCursorSelectPrev

forestCursorSelectNextTree :: ForestCursor a -> Maybe (ForestCursor a)
forestCursorSelectNextTree = forestCursorListCursorL nonEmptyCursorSelectNext

forestCursorSelectFirstTree :: ForestCursor a -> ForestCursor a
forestCursorSelectFirstTree =
    forestCursorListCursorL %~ nonEmptyCursorSelectFirst

forestCursorSelectLastTree :: ForestCursor a -> ForestCursor a
forestCursorSelectLastTree = forestCursorListCursorL %~ nonEmptyCursorSelectLast

forestCursorInsert :: ForestCursor a -> TreeCursor a -> ForestCursor a
forestCursorInsert fc tc =
    fc & forestCursorListCursorL %~ nonEmptyCursorInsert tc

forestCursorAppend :: ForestCursor a -> TreeCursor a -> ForestCursor a
forestCursorAppend fc tc =
    fc & forestCursorListCursorL %~ nonEmptyCursorAppend tc

forestCursorInsertAndSelect :: ForestCursor a -> TreeCursor a -> ForestCursor a
forestCursorInsertAndSelect fc tc =
    fc & forestCursorListCursorL %~ nonEmptyCursorInsertAndSelect tc

forestCursorAppendAndSelect :: ForestCursor a -> TreeCursor a -> ForestCursor a
forestCursorAppendAndSelect fc tc =
    fc & forestCursorListCursorL %~ nonEmptyCursorAppendAndSelect tc
