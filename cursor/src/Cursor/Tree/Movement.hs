{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.Tree.Movement
  ( treeCursorSelection
  , TreeCursorSelection(..)
  , treeCursorSelect
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
  , treeCursorSelectFirstOnSameLevel
  , treeCursorSelectLastOnSameLevel
  , treeCursorSelectAbovePrev
  , treeCursorSelectAboveNext
  ) where

import qualified Data.List.NonEmpty as NE
import Data.Validity.Tree ()

import Control.Applicative
import Control.Monad

import Cursor.Tree.Base
import Cursor.Tree.Types

treeCursorSelection :: TreeCursor a b -> TreeCursorSelection
treeCursorSelection TreeCursor {..} = wrap treeAbove SelectNode
  where
    wrap :: Maybe (TreeAbove a) -> TreeCursorSelection -> TreeCursorSelection
    wrap Nothing ts = ts
    wrap (Just ta) ts = wrap (treeAboveAbove ta) $ SelectChild (length $ treeAboveLefts ta) ts

treeCursorSelect ::
     (a -> b) -> (b -> a) -> TreeCursorSelection -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelect f g sel = makeTreeCursorWithSelection f g sel . rebuildTreeCursor f

treeCursorSelectPrev :: (a -> b) -> (b -> a) -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectPrev f g tc =
  treeCursorSelectAbovePrev f g tc <|> treeCursorSelectPrevOnSameLevel f g tc <|>
  treeCursorSelectAbove f g tc

treeCursorSelectNext :: (a -> b) -> (b -> a) -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectNext f g tc =
  treeCursorSelectBelowAtStart f g tc <|> treeCursorSelectNextOnSameLevel f g tc <|>
  treeCursorSelectAboveNext f g tc

treeCursorSelectFirst :: (a -> b) -> (b -> a) -> TreeCursor a b -> TreeCursor a b
treeCursorSelectFirst f g tc = maybe tc (treeCursorSelectFirst f g) $ treeCursorSelectPrev f g tc

treeCursorSelectLast :: (a -> b) -> (b -> a) -> TreeCursor a b -> TreeCursor a b
treeCursorSelectLast f g tc = maybe tc (treeCursorSelectLast f g) $ treeCursorSelectNext f g tc

treeCursorSelectAbove :: (a -> b) -> (b -> a) -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectAbove f g tc@TreeCursor {..} =
  case treeAbove of
    Nothing -> Nothing
    Just TreeAbove {..} ->
      let newForest = reverse treeAboveLefts ++ [currentTree f tc] ++ treeAboveRights
          newTree = CNode treeAboveNode $ openForest newForest
       in Just $ makeTreeCursorWithAbove g newTree treeAboveAbove

treeCursorSelectBelowAtPos ::
     (a -> b) -> (b -> a) -> Int -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectBelowAtPos f g pos TreeCursor {..} =
  case treeBelow of
    EmptyCForest -> Nothing
    ClosedForest _ -> Nothing
    OpenForest ts ->
      case splitAt pos $ NE.toList ts of
        (_, []) -> Nothing
        (lefts, current:rights) ->
          Just $
          makeTreeCursorWithAbove g current $
          Just $
          TreeAbove
            { treeAboveLefts = reverse lefts
            , treeAboveAbove = treeAbove
            , treeAboveNode = f treeCurrent
            , treeAboveRights = rights
            }

treeCursorSelectBelowAtStart :: (a -> b) -> (b -> a) -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectBelowAtStart f g = treeCursorSelectBelowAtPos f g 0

treeCursorSelectBelowAtEnd :: (a -> b) -> (b -> a) -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectBelowAtEnd f g tc =
  case treeBelow tc of
    EmptyCForest -> Nothing
    ClosedForest _ -> Nothing
    OpenForest ts -> treeCursorSelectBelowAtPos f g (length ts - 1) tc

treeCursorSelectBelowAtStartRecursively ::
     (a -> b) -> (b -> a) -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectBelowAtStartRecursively f g tc = go <$> treeCursorSelectBelowAtStart f g tc
  where
    go c = maybe c go $ treeCursorSelectBelowAtStart f g c

treeCursorSelectBelowAtEndRecursively ::
     (a -> b) -> (b -> a) -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectBelowAtEndRecursively f g tc = go <$> treeCursorSelectBelowAtEnd f g tc
  where
    go c = maybe c go $ treeCursorSelectBelowAtEnd f g c

treeCursorSelectPrevOnSameLevel :: (a -> b) -> (b -> a) -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectPrevOnSameLevel f g tc@TreeCursor {..} = do
  ta <- treeAbove
  case treeAboveLefts ta of
    [] -> Nothing
    tree:xs ->
      Just . makeTreeCursorWithAbove g tree $
      Just ta {treeAboveLefts = xs, treeAboveRights = currentTree f tc : treeAboveRights ta}

treeCursorSelectNextOnSameLevel :: (a -> b) -> (b -> a) -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectNextOnSameLevel f g tc@TreeCursor {..} = do
  ta <- treeAbove
  case treeAboveRights ta of
    [] -> Nothing
    tree:xs ->
      Just . makeTreeCursorWithAbove g tree . Just $
      ta {treeAboveLefts = currentTree f tc : treeAboveLefts ta, treeAboveRights = xs}

treeCursorSelectFirstOnSameLevel :: (a -> b) -> (b -> a) -> TreeCursor a b -> TreeCursor a b
treeCursorSelectFirstOnSameLevel f g tc =
  case treeCursorSelectPrevOnSameLevel f g tc of
    Nothing -> tc
    Just tc' -> treeCursorSelectFirstOnSameLevel f g tc'

treeCursorSelectLastOnSameLevel :: (a -> b) -> (b -> a) -> TreeCursor a b -> TreeCursor a b
treeCursorSelectLastOnSameLevel f g tc =
  case treeCursorSelectNextOnSameLevel f g tc of
    Nothing -> tc
    Just tc' -> treeCursorSelectLastOnSameLevel f g tc'

-- | Go back and down as far as necessary to find a previous element on a level below
treeCursorSelectAbovePrev :: (a -> b) -> (b -> a) -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectAbovePrev f g =
  treeCursorSelectPrevOnSameLevel f g >=> treeCursorSelectBelowAtEndRecursively f g

-- | Go up as far as necessary to find a next element on a level above and forward
--
-- Note: This will fail if there is a next node on the same level or any node below the current node
treeCursorSelectAboveNext :: (a -> b) -> (b -> a) -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectAboveNext f g tc =
  case treeCursorSelectNextOnSameLevel f g tc of
    Just _ -> Nothing
    Nothing ->
      case treeBelow tc of
        EmptyCForest -> go tc
        ClosedForest _ -> go tc
        OpenForest ts ->
          if null ts
            then go tc
            else Nothing
  where
    go tc_ = do
      tc' <- treeCursorSelectAbove f g tc_
      case treeCursorSelectNextOnSameLevel f g tc' of
        Nothing -> go tc'
        Just tc'' -> pure tc''
