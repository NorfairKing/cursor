{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.Tree.Draw
    ( drawTreeCursor
    , treeCursorWithPointer
    , showCForest
    , showCTree
    , showForest
    , showTree
    ) where

import qualified Data.List.NonEmpty as NE
import Data.Tree

import Cursor.Tree.Types

drawTreeCursor :: (Show a, Show b) => TreeCursor a b -> String
drawTreeCursor = drawTree . treeCursorWithPointer

treeCursorWithPointer :: (Show a, Show b) => TreeCursor a b -> Tree String
treeCursorWithPointer TreeCursor {..} =
    wrapAbove treeAbove $
    Node (show treeCurrent ++ " <---") $ showCForest treeBelow
  where
    wrapAbove :: (Show b) => Maybe (TreeAbove b) -> Tree String -> Tree String
    wrapAbove Nothing t = t
    wrapAbove (Just TreeAbove {..}) t =
        wrapAbove treeAboveAbove $
        Node (show treeAboveNode) $
        concat
            [ map showCTree $ reverse treeAboveLefts
            , [t]
            , map showCTree treeAboveRights
            ]

showCForest :: Show a => CForest a -> Forest String
showCForest EmptyCForest = []
showCForest (ClosedForest ts) =
    map (fmap ("hidden: " ++)) $ map showTree $ NE.toList ts
showCForest (OpenForest ts) = map showCTree $ NE.toList ts

showCTree :: Show a => CTree a -> Tree String
showCTree (CNode n fs) = Node (show n) $ showCForest fs

showForest :: Show a => Forest a -> Forest String
showForest = map showTree

showTree :: Show a => Tree a -> Tree String
showTree = fmap show
