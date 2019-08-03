{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.Tree.Swap
  ( treeCursorSwapPrev
  , treeCursorSwapNext
  , SwapResult(..)
  ) where

import qualified Data.Sequence as S
import Data.Sequence (ViewL(..), ViewR(..), (<|), (|>))
import Data.Validity

import GHC.Generics (Generic)

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
      case S.viewr $ treeAboveLefts ta of
        EmptyR -> NoSiblingsToSwapWith
        ts :> t ->
          Swapped $
          tc {treeAbove = Just ta {treeAboveLefts = ts, treeAboveRights = t <| treeAboveRights ta}}

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
      case S.viewl $ treeAboveRights ta of
        EmptyL -> NoSiblingsToSwapWith
        t :< ts ->
          Swapped $
          tc {treeAbove = Just ta {treeAboveLefts = treeAboveLefts ta |> t, treeAboveRights = ts}}

data SwapResult a
  = SwapperIsTopNode
  | NoSiblingsToSwapWith
  | Swapped a
  deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (SwapResult a)
