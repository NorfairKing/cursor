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
    , treeCursorPromoteElem
    , PromoteElemResult(..)
    , treeCursorPromoteSubTree
    , PromoteResult(..)
    -- * Demotions
    , treeCursorDemoteElem
    , treeCursorDemoteSubTree
    , DemoteResult(..)
    , treeCursorDemoteElemUnder
    , treeCursorDemoteSubTreeUnder
    -- * Collapse
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
import Cursor.Tree.Draw
import Cursor.Tree.Insert
import Cursor.Tree.Movement
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
       (a -> b)
    -> (b -> a)
    -> TreeCursor a b
    -> PromoteElemResult (TreeCursor a b)
treeCursorPromoteElem f g tc = do
    ta <-
        case treeAbove tc of
            Nothing -> CannotPromoteTopElem
            Just ta -> pure ta
    -- We need to put the below under the above lefts at the end
    lefts <-
        case (treeBelow tc) of
            ClosedForest [] -> pure $ treeAboveLefts ta
            _ ->
                case treeAboveLefts ta of
                    [] -> NoSiblingsToAdoptChildren
                    (CNode t ls:ts) ->
                        pure $
                        CNode
                            t
                            (openForest $
                             unpackCForest ls ++ unpackCForest (treeBelow tc)) :
                        ts
    taa <-
        case treeAboveAbove ta of
            Nothing -> NoGrandparentToPromoteElemUnder
            Just taa -> pure taa
    pure $
        makeTreeCursorWithAbove g (CNode (f $ treeCurrent tc) $ ClosedForest []) $
        Just $
        taa
        { treeAboveLefts =
              CNode
                  (treeAboveNode ta)
                  (openForest $ reverse lefts ++ treeAboveRights ta) :
              treeAboveLefts taa
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
    PromotedElem _ <*> NoGrandparentToPromoteElemUnder =
        NoGrandparentToPromoteElemUnder

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
treeCursorPromoteSubTree ::
       (a -> b) -> (b -> a) -> TreeCursor a b -> PromoteResult (TreeCursor a b)
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
              CNode
                  (treeAboveNode ta)
                  (openForest $
                   reverse (treeAboveLefts ta) ++ treeAboveRights ta) :
              treeAboveLefts taa
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
treeCursorDemoteElem ::
       (a -> b) -> (b -> a) -> TreeCursor a b -> DemoteResult (TreeCursor a b)
treeCursorDemoteElem f g tc =
    case treeAbove tc of
        Nothing -> CannotDemoteTopNode
        Just ta ->
            case treeAboveLefts ta of
                [] -> NoSiblingsToDemoteUnder
                (CNode t ls:ts) ->
                    Demoted $
                    makeTreeCursorWithAbove
                        g
                        (CNode (f $ treeCurrent tc) $ ClosedForest []) $
                    Just
                        TreeAbove
                        { treeAboveLefts = reverse $ unpackCForest ls
                        , treeAboveAbove = Just ta {treeAboveLefts = ts}
                        , treeAboveNode = t
                        , treeAboveRights = unpackCForest $ treeBelow tc
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
treeCursorDemoteSubTree ::
       (a -> b) -> (b -> a) -> TreeCursor a b -> DemoteResult (TreeCursor a b)
treeCursorDemoteSubTree f g tc =
    case treeAbove tc of
        Nothing -> CannotDemoteTopNode
        Just ta ->
            case treeAboveLefts ta of
                [] -> NoSiblingsToDemoteUnder
                (CNode t ls:ts) ->
                    Demoted $
                    makeTreeCursorWithAbove g (currentTree f tc) $
                    Just
                        TreeAbove
                        { treeAboveLefts = reverse $ unpackCForest ls
                        , treeAboveAbove = Just ta {treeAboveLefts = ts}
                        , treeAboveNode = t
                        , treeAboveRights = []
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
    let ta' =
            ta {treeAboveRights = CNode b2 (treeBelow tc) : treeAboveRights ta}
    pure
        tc
        { treeAbove =
              Just
                  TreeAbove
                  { treeAboveLefts = []
                  , treeAboveAbove = Just ta'
                  , treeAboveNode = b1
                  , treeAboveRights = []
                  }
        , treeBelow = ClosedForest []
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
              { treeAboveLefts = []
              , treeAboveAbove = treeAbove tc
              , treeAboveNode = b
              , treeAboveRights = []
              }
    }
