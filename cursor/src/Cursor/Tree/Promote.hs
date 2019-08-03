{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.Tree.Promote
  ( treeCursorPromoteElem
  , PromoteElemResult(..)
  , treeCursorPromoteSubTree
  , PromoteResult(..)
  ) where

import Data.Validity

import Data.Foldable
import qualified Data.Sequence as S
import Data.Sequence (ViewR(..), (<|), (><), (|>))
import GHC.Generics (Generic)

import Cursor.Tree.Base
import Cursor.Tree.Types

-- | Promotes the current node to the level of its parent.
--
-- Example:
--
-- Before:
--
-- >  p
-- >  |- a
-- >  |  |- b
-- >  |  |  |- c
-- >  |  |- d <--
-- >  |  |  |- e
-- >  |  |- f
-- >  |     |- g
-- >  |- h
--
-- After:
--
-- >  p
-- >  |- a
-- >  |  |- b
-- >  |  |  |- c
-- >  |  |  |- e
-- >  |  |- f
-- >  |     |- g
-- >  |- d <--
-- >  |- h
treeCursorPromoteElem ::
     (a -> b) -> (b -> a) -> TreeCursor a b -> PromoteElemResult (TreeCursor a b)
treeCursorPromoteElem f g tc = do
  ta <-
    case treeAbove tc of
      Nothing -> CannotPromoteTopElem
      Just ta -> pure ta
    -- We need to put the below under the above lefts at the end
  lefts <-
    case (treeBelow tc) of
      EmptyCForest -> pure $ treeAboveLefts ta
      _ ->
        case S.viewr $ treeAboveLefts ta of
          EmptyR -> NoSiblingsToAdoptChildren
          ts :> CNode t ls ->
            pure $ CNode t (openForest $ unpackCForest ls ++ unpackCForest (treeBelow tc)) <| ts
  taa <-
    case treeAboveAbove ta of
      Nothing -> NoGrandparentToPromoteElemUnder
      Just taa -> pure taa
  pure $
    makeTreeCursorWithAbove g (CNode (f $ treeCurrent tc) emptyCForest) $
    Just $
    taa
      { treeAboveLefts =
          treeAboveLefts taa |>
          CNode (treeAboveNode ta) (openForest $ toList $ lefts >< treeAboveRights ta)
      }

data PromoteElemResult a
  = CannotPromoteTopElem
  | NoGrandparentToPromoteElemUnder
  | NoSiblingsToAdoptChildren
  | PromotedElem a
  deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (PromoteElemResult a)

instance Applicative PromoteElemResult where
  pure = PromotedElem
  CannotPromoteTopElem <*> _ = CannotPromoteTopElem
  NoGrandparentToPromoteElemUnder <*> _ = NoGrandparentToPromoteElemUnder
  NoSiblingsToAdoptChildren <*> _ = NoSiblingsToAdoptChildren
  PromotedElem f <*> PromotedElem a = PromotedElem $ f a
  PromotedElem _ <*> CannotPromoteTopElem = CannotPromoteTopElem
  PromotedElem _ <*> NoSiblingsToAdoptChildren = NoSiblingsToAdoptChildren
  PromotedElem _ <*> NoGrandparentToPromoteElemUnder = NoGrandparentToPromoteElemUnder

instance Monad PromoteElemResult where
  CannotPromoteTopElem >>= _ = CannotPromoteTopElem
  NoGrandparentToPromoteElemUnder >>= _ = NoGrandparentToPromoteElemUnder
  NoSiblingsToAdoptChildren >>= _ = NoSiblingsToAdoptChildren
  PromotedElem a >>= f = f a

-- | Promotes the current node to the level of its parent.
--
-- Example:
--
-- Before:
--
-- >  p
-- >  |- a
-- >  |  |- b
-- >  |  |  |- c
-- >  |  |- d <--
-- >  |  |  |- e
-- >  |  |- f
-- >  |     |- g
-- >  |- h
--
-- After:
--
-- >  p
-- >  |- a
-- >  |  |- b
-- >  |  |  |- c
-- >  |  |- f
-- >  |     |- g
-- >  |- d <--
-- >  |  |- e
-- >  |- h
treeCursorPromoteSubTree :: (a -> b) -> (b -> a) -> TreeCursor a b -> PromoteResult (TreeCursor a b)
treeCursorPromoteSubTree f g tc = do
  ta <-
    case treeAbove tc of
      Nothing -> CannotPromoteTopNode
      Just ta -> pure ta
  taa <-
    case treeAboveAbove ta of
      Nothing -> NoGrandparentToPromoteUnder
      Just taa -> pure taa
  pure $
    makeTreeCursorWithAbove g (currentTree f tc) $
    Just $
    taa
      { treeAboveLefts =
          treeAboveLefts taa |>
          CNode (treeAboveNode ta) (openForest $ toList $ treeAboveLefts ta >< treeAboveRights ta)
      }

data PromoteResult a
  = CannotPromoteTopNode
  | NoGrandparentToPromoteUnder
  | Promoted a
  deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (PromoteResult a)

instance Applicative PromoteResult where
  pure = Promoted
  CannotPromoteTopNode <*> _ = CannotPromoteTopNode
  NoGrandparentToPromoteUnder <*> _ = NoGrandparentToPromoteUnder
  Promoted f <*> Promoted a = Promoted $ f a
  Promoted _ <*> CannotPromoteTopNode = CannotPromoteTopNode
  Promoted _ <*> NoGrandparentToPromoteUnder = NoGrandparentToPromoteUnder

instance Monad PromoteResult where
  CannotPromoteTopNode >>= _ = CannotPromoteTopNode
  NoGrandparentToPromoteUnder >>= _ = NoGrandparentToPromoteUnder
  Promoted a >>= f = f a
