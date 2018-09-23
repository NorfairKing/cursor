{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.Tree.Insert
    ( treeCursorInsert
    , treeCursorInsertAndSelect
    , treeCursorAppend
    , treeCursorAppendAndSelect
    , treeCursorAddChildAtPos
    , treeCursorAddChildAtStart
    , treeCursorAddChildAtEnd
    ) where

import Data.Tree
import Data.Validity
import Data.Validity.Tree ()

import GHC.Generics (Generic)

import Control.Applicative
import Control.Monad

import Lens.Micro

import Cursor.Tree.Base
import Cursor.Tree.Types
import Cursor.Types

treeCursorInsert :: Tree b -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorInsert tree tc@TreeCursor {..} = do
    ta <- treeAbove
    let newTreeAbove = ta {treeAboveLefts = makeCTree tree : treeAboveLefts ta}
    pure tc {treeAbove = Just newTreeAbove}

treeCursorInsertAndSelect ::
       (a -> b)
    -> (b -> a)
    -> Tree b
    -> TreeCursor a b
    -> Maybe (TreeCursor a b)
treeCursorInsertAndSelect f g tree tc@TreeCursor {..} = do
    ta <- treeAbove
    let newTreeAbove =
            ta {treeAboveRights = currentTree f tc : treeAboveRights ta}
    pure $ makeTreeCursorWithAbove g (makeCTree tree) $ Just newTreeAbove

treeCursorAppend :: Tree b -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorAppend tree tc@TreeCursor {..} = do
    ta <- treeAbove
    let newTreeAbove =
            ta {treeAboveRights = makeCTree tree : treeAboveRights ta}
    pure tc {treeAbove = Just newTreeAbove}

treeCursorAppendAndSelect ::
       (a -> b)
    -> (b -> a)
    -> Tree b
    -> TreeCursor a b
    -> Maybe (TreeCursor a b)
treeCursorAppendAndSelect f g tree tc@TreeCursor {..} = do
    ta <- treeAbove
    let newTreeAbove =
            ta {treeAboveLefts = currentTree f tc : treeAboveLefts ta}
    pure $ makeTreeCursorWithAbove g (makeCTree tree) $ Just newTreeAbove

treeCursorAddChildAtPos :: Int -> Tree b -> TreeCursor a b -> TreeCursor a b
treeCursorAddChildAtPos i t tc =
    let (before, after) = splitAt i $ collapseValue $ treeBelow tc
    in tc {treeBelow = collapse True $ before ++ [makeCTree t] ++ after}

treeCursorAddChildAtStart :: Tree b -> TreeCursor a b -> TreeCursor a b
treeCursorAddChildAtStart t tc =
    tc {treeBelow = collapse True $ makeCTree t : collapseValue (treeBelow tc)}

treeCursorAddChildAtEnd :: Tree b -> TreeCursor a b -> TreeCursor a b
treeCursorAddChildAtEnd t tc =
    tc
    {treeBelow = collapse True $ collapseValue (treeBelow tc) ++ [makeCTree t]}
