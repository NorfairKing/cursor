{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.Tree.Draw
    ( drawTreeCursor
    , treeCursorWithPointer
    , drawCollapseCForest
    , drawCForest
    , drawCTree
    ) where

import Data.Tree
import Data.Validity
import Data.Validity.Tree ()

import GHC.Generics (Generic)

import Control.Applicative
import Control.Monad

import Lens.Micro

import Cursor.Tree.Types
import Cursor.Types

drawTreeCursor :: (Show a, Show b) => TreeCursor a b -> String
drawTreeCursor = drawTree . treeCursorWithPointer

treeCursorWithPointer :: (Show a, Show b) => TreeCursor a b -> Tree String
treeCursorWithPointer TreeCursor {..} =
    wrapAbove treeAbove $
    Node (show treeCurrent ++ " <---") $ drawCollapseCForest treeBelow
  where
    wrapAbove :: (Show b) => Maybe (TreeAbove b) -> Tree String -> Tree String
    wrapAbove Nothing t = t
    wrapAbove (Just TreeAbove {..}) t =
        wrapAbove treeAboveAbove $
        Node (show treeAboveNode) $
        concat
            [ drawCForest $ reverse treeAboveLefts
            , [t]
            , drawCForest treeAboveRights
            ]

drawCollapseCForest :: Show a => Collapse (CForest a) -> Forest String
drawCollapseCForest Collapse {..} = drawCForest collapseValue

drawCForest :: Show a => CForest a -> Forest String
drawCForest = map drawCTree

drawCTree :: Show a => CTree a -> Tree String
drawCTree (CNode n fs) = Node (show n) $ drawCollapseCForest fs
