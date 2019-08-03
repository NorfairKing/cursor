{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.Tree.Demote
  ( treeCursorDemoteElem
  , treeCursorDemoteSubTree
  , DemoteResult(..)
  , treeCursorDemoteElemUnder
  , treeCursorDemoteSubTreeUnder
  ) where

import qualified Data.Sequence as S
import Data.Sequence (ViewR(..), (<|))
import Data.Validity

import GHC.Generics (Generic)

import Cursor.Tree.Base
import Cursor.Tree.Types

-- | Demotes the current node to the level of its children.
--
-- Example:
--
-- Before:
--
-- >  p
-- >  |- a
-- >  |  |- b
-- >  |- c <--
-- >  |  |- d
-- >  |- e
--
-- After:
--
-- >  p
-- >  |- a
-- >  |  |- b
-- >  |  |- c <--
-- >  |  |- d
-- >  |- e
treeCursorDemoteElem :: (a -> b) -> (b -> a) -> TreeCursor a b -> DemoteResult (TreeCursor a b)
treeCursorDemoteElem f g tc =
  case treeAbove tc of
    Nothing -> CannotDemoteTopNode
    Just ta ->
      case S.viewr $ treeAboveLefts ta of
        EmptyR -> NoSiblingsToDemoteUnder
        ts :> CNode t ls ->
          Demoted $
          makeTreeCursorWithAbove g (CNode (f $ treeCurrent tc) emptyCForest) $
          Just
            TreeAbove
              { treeAboveLefts = S.fromList $ unpackCForest ls
              , treeAboveAbove = Just ta {treeAboveLefts = ts}
              , treeAboveNode = t
              , treeAboveRights = S.fromList $ unpackCForest $ treeBelow tc
              }

-- | Demotes the current subtree to the level of its children.
--
-- Example:
--
-- Before:
--
-- >  p
-- >  |- a
-- >  |  |- b
-- >  |- c <--
-- >  |  |- d
-- >  |- e
--
-- After:
--
-- >  p
-- >  |- a
-- >  |  |- b
-- >  |  |- c <--
-- >  |     |- d
-- >  |- e
treeCursorDemoteSubTree :: (a -> b) -> (b -> a) -> TreeCursor a b -> DemoteResult (TreeCursor a b)
treeCursorDemoteSubTree f g tc =
  case treeAbove tc of
    Nothing -> CannotDemoteTopNode
    Just ta ->
      case S.viewr $ treeAboveLefts ta of
        EmptyR -> NoSiblingsToDemoteUnder
        ts :> CNode t ls ->
          Demoted $
          makeTreeCursorWithAbove g (currentTree f tc) $
          Just
            TreeAbove
              { treeAboveLefts = S.fromList $ unpackCForest ls
              , treeAboveAbove = Just ta {treeAboveLefts = ts}
              , treeAboveNode = t
              , treeAboveRights = S.empty
              }

data DemoteResult a
  = CannotDemoteTopNode
  | NoSiblingsToDemoteUnder
  | Demoted a
  deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (DemoteResult a)

-- | Demotes the current node to the level of its children, by adding two roots.
-- One for the current node and one for its children that are left behind.
--
-- Example:
--
-- Before:
--
-- >  p
-- >  |- a <--
-- >     |- b
--
-- After:
--
-- >  p
-- >  |- <given element 1>
-- >  |  |- a <--
-- >  |- <given element 2>
-- >  |  |- b
treeCursorDemoteElemUnder :: b -> b -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorDemoteElemUnder b1 b2 tc = do
  ta <- treeAbove tc
  let ta' = ta {treeAboveRights = CNode b2 (treeBelow tc) <| treeAboveRights ta}
  pure
    tc
      { treeAbove =
          Just
            TreeAbove
              { treeAboveLefts = S.empty
              , treeAboveAbove = Just ta'
              , treeAboveNode = b1
              , treeAboveRights = S.empty
              }
      , treeBelow = emptyCForest
      }

-- | Demotes the current subtree to the level of its children, by adding a root.
--
-- Example:
--
-- Before:
--
-- >  a <--
-- >  |- b
--
-- After:
--
-- >  <given element>
-- >  |- a <--
-- >     |- b
treeCursorDemoteSubTreeUnder :: b -> TreeCursor a b -> TreeCursor a b
treeCursorDemoteSubTreeUnder b tc =
  tc
    { treeAbove =
        Just
          TreeAbove
            { treeAboveLefts = S.empty
            , treeAboveAbove = treeAbove tc
            , treeAboveNode = b
            , treeAboveRights = S.empty
            }
    }
