{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.Tree.Insert
  ( treeCursorInsert,
    treeCursorInsertAndSelect,
    treeCursorInsertNodeSingleAndSelect,
    treeCursorInsertNodeAndSelect,
    treeCursorAppend,
    treeCursorAppendAndSelect,
    treeCursorAppendNodeSingleAndSelect,
    treeCursorAppendNodeAndSelect,
    treeCursorAddChildAtPos,
    treeCursorAddChildAtStart,
    treeCursorAddChildAtEnd,
    treeCursorAddChildAtPosAndSelect,
    treeCursorAddChildAtStartAndSelect,
    treeCursorAddChildAtEndAndSelect,
    treeCursorAddChildNodeSingleAtPosAndSelect,
    treeCursorAddChildNodeSingleAtStartAndSelect,
    treeCursorAddChildNodeSingleAtEndAndSelect,
    treeCursorAddChildNodeAtPosAndSelect,
    treeCursorAddChildNodeAtStartAndSelect,
    treeCursorAddChildNodeAtEndAndSelect,
  )
where

import Cursor.Tree.Base
import Cursor.Tree.Types
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty ((<|))
import Data.Tree

treeCursorInsert :: Tree b -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorInsert tree tc@TreeCursor {..} = do
  ta <- treeAbove
  let newTreeAbove = ta {treeAboveLefts = makeCTree tree : treeAboveLefts ta}
  pure tc {treeAbove = Just newTreeAbove}

treeCursorInsertAndSelect ::
  (a -> b) -> (b -> a) -> Tree b -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorInsertAndSelect f g (Node value forest) = treeCursorInsertNodeAndSelect f (g value) (makeCForest forest)

treeCursorInsertNodeSingleAndSelect ::
  (a -> b) -> a -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorInsertNodeSingleAndSelect f a = treeCursorInsertNodeAndSelect f a EmptyCForest

treeCursorInsertNodeAndSelect ::
  (a -> b) -> a -> CForest b -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorInsertNodeAndSelect f value forest tc = do
  ta <- treeAbove tc
  let ta' = ta {treeAboveRights = CNode (f (treeCurrent tc)) (treeBelow tc) : treeAboveRights ta}
      tc' = tc {treeAbove = Just ta', treeCurrent = value, treeBelow = forest}
  pure tc'

treeCursorAppend :: Tree b -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorAppend tree tc@TreeCursor {..} = do
  ta <- treeAbove
  let newTreeAbove = ta {treeAboveRights = makeCTree tree : treeAboveRights ta}
  pure tc {treeAbove = Just newTreeAbove}

treeCursorAppendAndSelect ::
  (a -> b) -> (b -> a) -> Tree b -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorAppendAndSelect f g (Node value forest) = treeCursorAppendNodeAndSelect f (g value) (makeCForest forest)

treeCursorAppendNodeSingleAndSelect ::
  (a -> b) -> a -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorAppendNodeSingleAndSelect f a = treeCursorAppendNodeAndSelect f a EmptyCForest

treeCursorAppendNodeAndSelect ::
  (a -> b) -> a -> CForest b -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorAppendNodeAndSelect f value forest tc = do
  ta <- treeAbove tc
  let ta' = ta {treeAboveLefts = CNode (f (treeCurrent tc)) (treeBelow tc) : treeAboveLefts ta}
      tc' = tc {treeAbove = Just ta', treeCurrent = value, treeBelow = forest}
  pure tc'

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
    ClosedForest ts -> tc {treeBelow = openForest $ map makeCTree $ NE.toList ts ++ [t]}
    OpenForest ts -> tc {treeBelow = openForest $ NE.toList ts ++ [makeCTree t]}

treeCursorAddChildAtPosAndSelect ::
  (a -> b) -> (b -> a) -> Int -> Tree b -> TreeCursor a b -> TreeCursor a b
treeCursorAddChildAtPosAndSelect f g i (Node t ts) = treeCursorAddChildNodeAtPosAndSelect f i (g t) ts

treeCursorAddChildAtStartAndSelect ::
  (a -> b) -> (b -> a) -> Tree b -> TreeCursor a b -> TreeCursor a b
treeCursorAddChildAtStartAndSelect f g (Node t ts) = treeCursorAddChildNodeAtStartAndSelect f (g t) ts

treeCursorAddChildAtEndAndSelect ::
  (a -> b) -> (b -> a) -> Tree b -> TreeCursor a b -> TreeCursor a b
treeCursorAddChildAtEndAndSelect f g (Node t ts) = treeCursorAddChildNodeAtEndAndSelect f (g t) ts

treeCursorAddChildNodeSingleAtPosAndSelect ::
  (a -> b) -> Int -> a -> TreeCursor a b -> TreeCursor a b
treeCursorAddChildNodeSingleAtPosAndSelect f i a = treeCursorAddChildNodeAtPosAndSelect f i a []

treeCursorAddChildNodeSingleAtStartAndSelect ::
  (a -> b) -> a -> TreeCursor a b -> TreeCursor a b
treeCursorAddChildNodeSingleAtStartAndSelect f a = treeCursorAddChildNodeAtStartAndSelect f a []

treeCursorAddChildNodeSingleAtEndAndSelect ::
  (a -> b) -> a -> TreeCursor a b -> TreeCursor a b
treeCursorAddChildNodeSingleAtEndAndSelect f a = treeCursorAddChildNodeAtEndAndSelect f a []

treeCursorAddChildNodeAtPosAndSelect ::
  (a -> b) -> Int -> a -> Forest b -> TreeCursor a b -> TreeCursor a b
treeCursorAddChildNodeAtPosAndSelect f i t ts tc =
  let (before, after) = splitAt i $ unpackCForest $ treeBelow tc
   in TreeCursor
        { treeAbove =
            Just
              TreeAbove
                { treeAboveLefts = reverse before,
                  treeAboveAbove = treeAbove tc,
                  treeAboveNode = f (treeCurrent tc),
                  treeAboveRights = after
                },
          treeCurrent = t,
          treeBelow = makeCForest ts
        }

treeCursorAddChildNodeAtStartAndSelect ::
  (a -> b) -> a -> Forest b -> TreeCursor a b -> TreeCursor a b
treeCursorAddChildNodeAtStartAndSelect f t ts tc =
  TreeCursor
    { treeAbove =
        Just
          TreeAbove
            { treeAboveLefts = [],
              treeAboveAbove = treeAbove tc,
              treeAboveNode = f (treeCurrent tc),
              treeAboveRights = unpackCForest $ treeBelow tc
            },
      treeCurrent = t,
      treeBelow = makeCForest ts
    }

treeCursorAddChildNodeAtEndAndSelect ::
  (a -> b) -> a -> Forest b -> TreeCursor a b -> TreeCursor a b
treeCursorAddChildNodeAtEndAndSelect f t ts tc =
  TreeCursor
    { treeAbove =
        Just
          TreeAbove
            { treeAboveLefts = reverse $ unpackCForest $ treeBelow tc,
              treeAboveAbove = treeAbove tc,
              treeAboveNode = f (treeCurrent tc),
              treeAboveRights = []
            },
      treeCurrent = t,
      treeBelow = makeCForest ts
    }
