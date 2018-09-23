{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Cursor.Tree
    ( TreeCursor(..)
    , TreeAbove(..)
    -- * Types
    , module Cursor.Tree.Types
    -- * Construction, destruction
    , module Cursor.Tree.Base
    -- * Drawing
    , module Cursor.Tree.Draw
    -- * Movements
    , module Cursor.Tree.Movement
    -- * Insertions
    , module Cursor.Tree.Insert
    -- * Deletions
    , module Cursor.Tree.Delete
    -- * Swapping
    , treeCursorSwapPrev
    , treeCursorSwapNext
    , SwapResult(..)
    -- * Promotions
    , module Cursor.Tree.Promote
    -- * Demotions
    , module Cursor.Tree.Demote
    -- * CTree
    , CTree(..)
    , makeCTree
    , cTree
    , rebuildCTree
    , CForest(..)
    , makeCForest
    , cForest
    , rebuildCForest
    ) where

import Data.Validity

import GHC.Generics (Generic)

import Cursor.Tree.Base
import Cursor.Tree.Delete
import Cursor.Tree.Demote
import Cursor.Tree.Draw
import Cursor.Tree.Insert
import Cursor.Tree.Movement
import Cursor.Tree.Promote
import Cursor.Tree.Types

-- | Swaps the current node with the previous node on the same level
--
-- Example:
--
-- Before:
--
-- > p
-- > |- a
-- > |- b <--
--
-- After:
--
-- > p
-- > |- b <--
-- > |- a
treeCursorSwapPrev :: TreeCursor a b -> SwapResult (TreeCursor a b)
treeCursorSwapPrev tc = do
    case treeAbove tc of
        Nothing -> SwapperIsTopNode
        Just ta ->
            case treeAboveLefts ta of
                [] -> NoSiblingsToSwapWith
                (t:ts) ->
                    Swapped $
                    tc
                    { treeAbove =
                          Just
                              ta
                              { treeAboveLefts = ts
                              , treeAboveRights = t : treeAboveRights ta
                              }
                    }

-- | Swaps the current node with the next node on the same level
--
-- Example:
--
-- Before:
--
-- > p
-- > |- a <--
-- > |- b
--
-- After:
--
-- > p
-- > |- b
-- > |- a <--
treeCursorSwapNext :: TreeCursor a b -> SwapResult (TreeCursor a b)
treeCursorSwapNext tc =
    case treeAbove tc of
        Nothing -> SwapperIsTopNode
        Just ta ->
            case treeAboveRights ta of
                [] -> NoSiblingsToSwapWith
                (t:ts) ->
                    Swapped $
                    tc
                    { treeAbove =
                          Just
                              ta
                              { treeAboveLefts = t : treeAboveLefts ta
                              , treeAboveRights = ts
                              }
                    }

data SwapResult a
    = SwapperIsTopNode
    | NoSiblingsToSwapWith
    | Swapped a
    deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (SwapResult a)
