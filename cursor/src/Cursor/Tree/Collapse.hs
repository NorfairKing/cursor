module Cursor.Tree.Collapse
  ( treeCursorOpenCurrentForest,
    treeCursorCloseCurrentForest,
    treeCursorToggleCurrentForest,
    treeCursorOpenCurrentForestRecursively,
    treeCursorToggleCurrentForestRecursively,
  )
where

import Cursor.Tree.Types
import qualified Data.List.NonEmpty as NE

treeCursorOpenCurrentForest :: TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorOpenCurrentForest tc =
  case treeBelow tc of
    EmptyCForest -> Nothing
    ClosedForest ts -> Just $ tc {treeBelow = OpenForest $ NE.map makeCTree ts}
    OpenForest _ -> Nothing

treeCursorCloseCurrentForest :: TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorCloseCurrentForest tc =
  case treeBelow tc of
    EmptyCForest -> Nothing
    ClosedForest _ -> Nothing
    OpenForest ts -> Just $ tc {treeBelow = ClosedForest $ NE.map rebuildCTree ts}

treeCursorToggleCurrentForest :: TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorToggleCurrentForest tc =
  case treeBelow tc of
    EmptyCForest -> Nothing
    ClosedForest ts -> Just $ tc {treeBelow = OpenForest $ NE.map makeCTree ts}
    OpenForest ts -> Just $ tc {treeBelow = ClosedForest $ NE.map rebuildCTree ts}

treeCursorOpenCurrentForestRecursively :: TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorOpenCurrentForestRecursively tc =
  case treeBelow tc of
    EmptyCForest -> Nothing
    ClosedForest ts -> Just $ tc {treeBelow = OpenForest $ NE.map (cTree True) ts}
    OpenForest _ -> Nothing

treeCursorToggleCurrentForestRecursively :: TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorToggleCurrentForestRecursively tc =
  case treeBelow tc of
    EmptyCForest -> Nothing
    ClosedForest ts -> Just $ tc {treeBelow = OpenForest $ NE.map (cTree True) ts}
    OpenForest ts -> Just $ tc {treeBelow = ClosedForest $ NE.map rebuildCTree ts}
