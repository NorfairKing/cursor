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

import GHC.Generics (Generic)

import Data.Validity

import Control.DeepSeq

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
        case treeAboveLefts ta of
          [] -> NoSiblingsToAdoptChildren
          (CNode t ls:ts) ->
            pure $ CNode t (openForest $ unpackCForest ls ++ unpackCForest (treeBelow tc)) : ts
  taa <-
    case treeAboveAbove ta of
      Nothing -> NoGrandparentToPromoteElemUnder
      Just taa -> pure taa
  pure $
    makeTreeCursorWithAbove g (CNode (f $ treeCurrent tc) emptyCForest) $
    Just $
    taa
      { treeAboveLefts =
          CNode (treeAboveNode ta) (openForest $ reverse lefts ++ treeAboveRights ta) :
          treeAboveLefts taa
      }

data PromoteElemResult a
  = CannotPromoteTopElem
  | NoGrandparentToPromoteElemUnder
  | NoSiblingsToAdoptChildren
  | PromotedElem a
  deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (PromoteElemResult a)

instance NFData a => NFData (PromoteElemResult a)

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
          CNode (treeAboveNode ta) (openForest $ reverse (treeAboveLefts ta) ++ treeAboveRights ta) :
          treeAboveLefts taa
      }

data PromoteResult a
  = CannotPromoteTopNode
  | NoGrandparentToPromoteUnder
  | Promoted a
  deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (PromoteResult a)
instance NFData a => NFData (PromoteResult a)

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
