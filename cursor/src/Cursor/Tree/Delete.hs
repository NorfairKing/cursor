{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.Tree.Delete
  ( treeCursorDeleteSubTreeAndSelectPrevious,
    treeCursorDeleteSubTreeAndSelectNext,
    treeCursorDeleteSubTreeAndSelectAbove,
    treeCursorRemoveSubTree,
    treeCursorDeleteSubTree,
    treeCursorDeleteElemAndSelectPrevious,
    treeCursorDeleteElemAndSelectNext,
    treeCursorDeleteElemAndSelectAbove,
    treeCursorRemoveElem,
    treeCursorDeleteElem,
  )
where

import Control.Applicative
import Cursor.Tree.Base
import Cursor.Tree.Types
import Cursor.Types
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Tree

treeCursorDeleteSubTreeAndSelectPrevious ::
  (b -> a) -> TreeCursor a b -> Maybe (DeleteOrUpdate (TreeCursor a b))
treeCursorDeleteSubTreeAndSelectPrevious g TreeCursor {..} =
  case treeAbove of
    Nothing -> Just Deleted
    Just ta ->
      case treeAboveLefts ta of
        [] -> Nothing
        tree : xs -> Just . Updated . makeTreeCursorWithAbove g tree $ Just ta {treeAboveLefts = xs}

treeCursorDeleteSubTreeAndSelectNext ::
  (b -> a) -> TreeCursor a b -> Maybe (DeleteOrUpdate (TreeCursor a b))
treeCursorDeleteSubTreeAndSelectNext g TreeCursor {..} =
  case treeAbove of
    Nothing -> Just Deleted
    Just ta ->
      case treeAboveRights ta of
        [] -> Nothing
        tree : xs -> Just . Updated . makeTreeCursorWithAbove g tree $ Just ta {treeAboveRights = xs}

treeCursorDeleteSubTreeAndSelectAbove ::
  (b -> a) -> TreeCursor a b -> DeleteOrUpdate (TreeCursor a b)
treeCursorDeleteSubTreeAndSelectAbove g TreeCursor {..} =
  case treeAbove of
    Nothing -> Deleted
    Just TreeAbove {..} ->
      Updated $
        TreeCursor
          { treeAbove = treeAboveAbove,
            treeCurrent = g treeAboveNode,
            treeBelow = openForest $ reverse treeAboveLefts ++ treeAboveRights
          }

treeCursorRemoveSubTree :: (b -> a) -> TreeCursor a b -> DeleteOrUpdate (TreeCursor a b)
treeCursorRemoveSubTree g tc =
  joinDeletes
    (treeCursorDeleteSubTreeAndSelectPrevious g tc)
    (treeCursorDeleteSubTreeAndSelectNext g tc)
    <|> treeCursorDeleteSubTreeAndSelectAbove g tc

treeCursorDeleteSubTree :: (b -> a) -> TreeCursor a b -> DeleteOrUpdate (TreeCursor a b)
treeCursorDeleteSubTree g tc =
  joinDeletes
    (treeCursorDeleteSubTreeAndSelectNext g tc)
    (treeCursorDeleteSubTreeAndSelectPrevious g tc)
    <|> treeCursorDeleteSubTreeAndSelectAbove g tc

treeCursorDeleteElemAndSelectPrevious ::
  (b -> a) -> TreeCursor a b -> Maybe (DeleteOrUpdate (TreeCursor a b))
treeCursorDeleteElemAndSelectPrevious g TreeCursor {..} =
  case treeAbove of
    Nothing ->
      case treeBelow of
        EmptyCForest -> Just Deleted
        _ -> Nothing
    Just ta ->
      case treeAboveLefts ta of
        [] -> Nothing
        tree : xs ->
          Just . Updated . makeTreeCursorWithAbove g tree $
            Just
              ta
                { treeAboveLefts = xs,
                  treeAboveRights = unpackCForest treeBelow ++ treeAboveRights ta
                }

treeCursorDeleteElemAndSelectNext ::
  (b -> a) -> TreeCursor a b -> Maybe (DeleteOrUpdate (TreeCursor a b))
treeCursorDeleteElemAndSelectNext g TreeCursor {..} =
  case treeBelow of
    EmptyCForest ->
      case treeAbove of
        Nothing -> Just Deleted
        Just ta ->
          case treeAboveRights ta of
            [] -> Nothing
            tree : xs ->
              Just . Updated . makeTreeCursorWithAbove g tree $ Just ta {treeAboveRights = xs}
    ClosedForest ts ->
      case treeAbove of
        Nothing ->
          case ts of
            (Node e ts_ :| xs) ->
              let t = CNode e $ closedForest $ ts_ ++ xs
               in Just . Updated $ makeTreeCursorWithAbove g t treeAbove
        Just ta ->
          case treeAboveRights ta of
            [] -> Nothing
            tree : xs ->
              Just . Updated . makeTreeCursorWithAbove g tree $
                Just
                  ta
                    { treeAboveLefts = map makeCTree (reverse $ NE.toList ts) ++ treeAboveLefts ta,
                      treeAboveRights = xs
                    }
    OpenForest (CNode e ts :| xs) ->
      let t =
            CNode e $
              case ts of
                EmptyCForest -> openForest xs
                OpenForest ts_ -> openForest $ NE.toList ts_ ++ xs
                ClosedForest ts_ -> closedForest $ NE.toList ts_ ++ map rebuildCTree xs
       in Just . Updated $ makeTreeCursorWithAbove g t treeAbove

treeCursorDeleteElemAndSelectAbove ::
  (b -> a) -> TreeCursor a b -> Maybe (DeleteOrUpdate (TreeCursor a b))
treeCursorDeleteElemAndSelectAbove g TreeCursor {..} =
  case treeAbove of
    Nothing ->
      case treeBelow of
        EmptyCForest -> Just Deleted
        _ -> Nothing
    Just TreeAbove {..} ->
      Just $
        Updated $
          TreeCursor
            { treeAbove = treeAboveAbove,
              treeCurrent = g treeAboveNode,
              treeBelow =
                openForest $ reverse treeAboveLefts ++ unpackCForest treeBelow ++ treeAboveRights
            }

treeCursorRemoveElem :: (b -> a) -> TreeCursor a b -> DeleteOrUpdate (TreeCursor a b)
treeCursorRemoveElem g tc =
  joinDeletes3
    (treeCursorDeleteElemAndSelectPrevious g tc)
    (treeCursorDeleteElemAndSelectNext g tc)
    (treeCursorDeleteElemAndSelectAbove g tc)

treeCursorDeleteElem :: (b -> a) -> TreeCursor a b -> DeleteOrUpdate (TreeCursor a b)
treeCursorDeleteElem g tc =
  joinDeletes3
    (treeCursorDeleteElemAndSelectNext g tc)
    (treeCursorDeleteElemAndSelectPrevious g tc)
    (treeCursorDeleteElemAndSelectAbove g tc)
