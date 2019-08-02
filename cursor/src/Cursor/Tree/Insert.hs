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

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty ((<|))
import Data.Tree

import Cursor.Tree.Base
import Cursor.Tree.Types

treeCursorInsert :: Tree b -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorInsert tree tc@TreeCursor {..} = do
  ta <- treeAbove
  let newTreeAbove = ta {treeAboveLefts = makeCTree tree : treeAboveLefts ta}
  pure tc {treeAbove = Just newTreeAbove}

treeCursorInsertAndSelect ::
     (a -> b) -> (b -> a) -> Tree b -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorInsertAndSelect f g tree tc@TreeCursor {..} = do
  ta <- treeAbove
  let newTreeAbove =
        ta {treeAboveRights = currentTree f tc : treeAboveRights ta}
  pure $ makeTreeCursorWithAbove g (makeCTree tree) $ Just newTreeAbove

treeCursorAppend :: Tree b -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorAppend tree tc@TreeCursor {..} = do
  ta <- treeAbove
  let newTreeAbove = ta {treeAboveRights = makeCTree tree : treeAboveRights ta}
  pure tc {treeAbove = Just newTreeAbove}

treeCursorAppendAndSelect ::
     (a -> b) -> (b -> a) -> Tree b -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorAppendAndSelect f g tree tc@TreeCursor {..} = do
  ta <- treeAbove
  let newTreeAbove = ta {treeAboveLefts = currentTree f tc : treeAboveLefts ta}
  pure $ makeTreeCursorWithAbove g (makeCTree tree) $ Just newTreeAbove

-- TODO make this fail if the position doesn't make sense
treeCursorAddChildAtPos :: Int -> Tree b -> TreeCursor a b -> TreeCursor a b
treeCursorAddChildAtPos i t tc =
  case treeBelow tc of
    EmptyCForest -> tc {treeBelow = openForest [makeCTree t]}
    ClosedForest ts ->
      let (before, after) = splitAt i $ NE.toList ts
       in tc {treeBelow = openForest $ map makeCTree $ before ++ [t] ++ after}
    OpenForest ts ->
      let (before, after) = splitAt i $ NE.toList ts
       in tc {treeBelow = openForest $ before ++ [makeCTree t] ++ after}

treeCursorAddChildAtStart :: Tree b -> TreeCursor a b -> TreeCursor a b
treeCursorAddChildAtStart t tc =
  case treeBelow tc of
    EmptyCForest -> tc {treeBelow = openForest [makeCTree t]}
    ClosedForest ts -> tc {treeBelow = OpenForest $ NE.map makeCTree $ t <| ts}
    OpenForest ts -> tc {treeBelow = OpenForest $ makeCTree t <| ts}

treeCursorAddChildAtEnd :: Tree b -> TreeCursor a b -> TreeCursor a b
treeCursorAddChildAtEnd t tc =
  case treeBelow tc of
    EmptyCForest -> tc {treeBelow = openForest [makeCTree t]}
    ClosedForest ts ->
      tc {treeBelow = openForest $ map makeCTree $ NE.toList ts ++ [t]}
    OpenForest ts -> tc {treeBelow = openForest $ NE.toList ts ++ [makeCTree t]}
