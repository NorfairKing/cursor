module Cursor.Tree.Collapse
    ( treeCursorCloseCurrentForest
    , treeCursorOpenCurrentForest
    , treeCursorToggleCurrentForest
    ) where

import qualified Data.List.NonEmpty as NE

import Cursor.Tree.Types

treeCursorOpenCurrentForest :: TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorOpenCurrentForest tc =
    case treeBelow tc of
        EmptyCForest -> Nothing
        ClosedForest ts ->
            Just $ tc {treeBelow = OpenForest $ NE.map makeCTree ts}
        OpenForest _ -> Nothing

treeCursorCloseCurrentForest :: TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorCloseCurrentForest tc =
    case treeBelow tc of
        EmptyCForest -> Nothing
        ClosedForest _ -> Nothing
        OpenForest ts ->
            Just $ tc {treeBelow = ClosedForest $ NE.map rebuildCTree ts}

treeCursorToggleCurrentForest :: TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorToggleCurrentForest tc =
    case treeBelow tc of
        EmptyCForest -> Nothing
        ClosedForest ts ->
            Just $ tc {treeBelow = OpenForest $ NE.map makeCTree ts}
        OpenForest ts ->
            Just $ tc {treeBelow = ClosedForest $ NE.map rebuildCTree ts}
