{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.Tree.Delete
    ( treeCursorDeleteSubTreeAndSelectPrevious
    , treeCursorDeleteSubTreeAndSelectNext
    , treeCursorDeleteSubTreeAndSelectAbove
    , treeCursorRemoveSubTree
    , treeCursorDeleteSubTree
    , treeCursorDeleteElemAndSelectPrevious
    , treeCursorDeleteElemAndSelectNext
    , treeCursorDeleteElemAndSelectAbove
    , treeCursorRemoveElem
    , treeCursorDeleteElem
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

treeCursorDeleteSubTreeAndSelectPrevious ::
       (b -> a) -> TreeCursor a b -> Maybe (DeleteOrUpdate (TreeCursor a b))
treeCursorDeleteSubTreeAndSelectPrevious g TreeCursor {..} =
    case treeAbove of
        Nothing -> Just Deleted
        Just ta ->
            case treeAboveLefts ta of
                [] -> Nothing
                tree:xs ->
                    Just . Updated . makeTreeCursorWithAbove g tree $
                    Just ta {treeAboveLefts = xs}

treeCursorDeleteSubTreeAndSelectNext ::
       (b -> a) -> TreeCursor a b -> Maybe (DeleteOrUpdate (TreeCursor a b))
treeCursorDeleteSubTreeAndSelectNext g TreeCursor {..} =
    case treeAbove of
        Nothing -> Just Deleted
        Just ta ->
            case treeAboveRights ta of
                [] -> Nothing
                tree:xs ->
                    Just . Updated . makeTreeCursorWithAbove g tree $
                    Just ta {treeAboveRights = xs}

treeCursorDeleteSubTreeAndSelectAbove ::
       (b -> a) -> TreeCursor a b -> DeleteOrUpdate (TreeCursor a b)
treeCursorDeleteSubTreeAndSelectAbove g TreeCursor {..} =
    case treeAbove of
        Nothing -> Deleted
        Just TreeAbove {..} ->
            Updated $
            TreeCursor
            { treeAbove = treeAboveAbove
            , treeCurrent = g treeAboveNode
            , treeBelow =
                  OpenForest $ (reverse treeAboveLefts) ++ treeAboveRights
            }

treeCursorRemoveSubTree ::
       (b -> a) -> TreeCursor a b -> DeleteOrUpdate (TreeCursor a b)
treeCursorRemoveSubTree g tc =
    joinDeletes
        (treeCursorDeleteSubTreeAndSelectPrevious g tc)
        (treeCursorDeleteSubTreeAndSelectNext g tc) <|>
    treeCursorDeleteSubTreeAndSelectAbove g tc

treeCursorDeleteSubTree ::
       (b -> a) -> TreeCursor a b -> DeleteOrUpdate (TreeCursor a b)
treeCursorDeleteSubTree g tc =
    joinDeletes
        (treeCursorDeleteSubTreeAndSelectNext g tc)
        (treeCursorDeleteSubTreeAndSelectPrevious g tc) <|>
    treeCursorDeleteSubTreeAndSelectAbove g tc

treeCursorDeleteElemAndSelectPrevious ::
       (b -> a) -> TreeCursor a b -> Maybe (DeleteOrUpdate (TreeCursor a b))
treeCursorDeleteElemAndSelectPrevious g TreeCursor {..} =
    case treeAbove of
        Nothing ->
            case treeBelow of
                ClosedForest [] -> Just Deleted
                OpenForest [] -> Just Deleted
                _ -> Nothing
        Just ta ->
            case treeAboveLefts ta of
                [] -> Nothing
                tree:xs ->
                    Just . Updated . makeTreeCursorWithAbove g tree $
                    Just
                        ta
                        { treeAboveLefts = xs
                        , treeAboveRights =
                              (case treeBelow of
                                   OpenForest ts -> ts
                                   ClosedForest ts -> map makeCTree ts) ++
                              treeAboveRights ta
                        }

treeCursorDeleteElemAndSelectNext ::
       (b -> a) -> TreeCursor a b -> Maybe (DeleteOrUpdate (TreeCursor a b))
treeCursorDeleteElemAndSelectNext g TreeCursor {..} =
    case treeBelow of
        OpenForest [] ->
            case treeAbove of
                Nothing -> Just Deleted
                Just ta ->
                    case treeAboveRights ta of
                        [] -> Nothing
                        tree:xs ->
                            Just . Updated . makeTreeCursorWithAbove g tree $
                            Just ta {treeAboveRights = xs}
        ClosedForest ts ->
            case treeAbove of
                Nothing ->
                    case ts of
                        [] -> Just Deleted
                        (Node e ts:xs) ->
                            let t =
                                    CNode e $
                                    OpenForest $ map makeCTree $ ts ++ xs
                            in Just . Updated $
                               makeTreeCursorWithAbove g t treeAbove
                Just ta ->
                    case treeAboveRights ta of
                        [] -> Nothing
                        tree:xs ->
                            Just . Updated . makeTreeCursorWithAbove g tree $
                            Just
                                ta
                                { treeAboveLefts =
                                      map makeCTree (reverse ts) ++
                                      treeAboveLefts ta
                                , treeAboveRights = xs
                                }
        OpenForest (CNode e ts:xs) ->
            let t = CNode e $ OpenForest $ unpackCForest ts ++ xs
            in Just . Updated $ makeTreeCursorWithAbove g t treeAbove

treeCursorDeleteElemAndSelectAbove ::
       (b -> a) -> TreeCursor a b -> Maybe (DeleteOrUpdate (TreeCursor a b))
treeCursorDeleteElemAndSelectAbove g TreeCursor {..} =
    case treeAbove of
        Nothing ->
            case treeBelow of
                ClosedForest [] -> Just Deleted
                OpenForest [] -> Just Deleted
                _ -> Nothing
        Just TreeAbove {..} ->
            Just $
            Updated $
            TreeCursor
            { treeAbove = treeAboveAbove
            , treeCurrent = g treeAboveNode
            , treeBelow =
                  OpenForest $
                  reverse treeAboveLefts ++
                  (case treeBelow of
                       OpenForest ts -> ts
                       ClosedForest ts -> map makeCTree ts) ++
                  treeAboveRights
            }

treeCursorRemoveElem ::
       (b -> a) -> TreeCursor a b -> DeleteOrUpdate (TreeCursor a b)
treeCursorRemoveElem g tc =
    joinDeletes3
        (treeCursorDeleteElemAndSelectPrevious g tc)
        (treeCursorDeleteElemAndSelectNext g tc)
        (treeCursorDeleteElemAndSelectAbove g tc)

treeCursorDeleteElem ::
       (b -> a) -> TreeCursor a b -> DeleteOrUpdate (TreeCursor a b)
treeCursorDeleteElem g tc =
    joinDeletes3
        (treeCursorDeleteElemAndSelectNext g tc)
        (treeCursorDeleteElemAndSelectPrevious g tc)
        (treeCursorDeleteElemAndSelectAbove g tc)
