{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.Tree.Swap
  ( treeCursorSwapPrev,
    treeCursorSwapNext,
    SwapResult (..),
  )
where

import Control.DeepSeq
import Cursor.Tree.Types
import Data.Validity
import GHC.Generics (Generic)

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
treeCursorSwapPrev tc =
  case treeAbove tc of
    Nothing -> SwapperIsTopNode
    Just ta ->
      case treeAboveLefts ta of
        [] -> NoSiblingsToSwapWith
        (t : ts) ->
          Swapped $
            tc {treeAbove = Just ta {treeAboveLefts = ts, treeAboveRights = t : treeAboveRights ta}}

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
        (t : ts) ->
          Swapped $
            tc {treeAbove = Just ta {treeAboveLefts = t : treeAboveLefts ta, treeAboveRights = ts}}

data SwapResult a
  = SwapperIsTopNode
  | NoSiblingsToSwapWith
  | Swapped a
  deriving (Show, Eq, Generic, Functor)

instance (Validity a) => Validity (SwapResult a)

instance (NFData a) => NFData (SwapResult a)
