{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.Tree where

import Data.Tree
import Data.Validity
import Data.Validity.Tree ()
import GHC.Generics (Generic)

data TreeCursor a = TreeCursor
    { treeAbove :: Maybe (TreeAbove a)
    , treeCurrent :: a
    , treeBelow :: Forest a
    } deriving (Show, Eq, Generic)

instance Validity a => Validity (TreeCursor a)

instance Functor TreeCursor where
    fmap f tc =
        TreeCursor
            { treeAbove = fmap (fmap f) $ treeAbove tc
            , treeCurrent = f $ treeCurrent tc
            , treeBelow = map (fmap f) $ treeBelow tc
            }

data TreeAbove a = TreeAbove
    { treeAboveLefts :: [Tree a]
    , treeAboveAbove :: Maybe (TreeAbove a)
    , treeAboveNode :: a
    , treeAboveRights :: [Tree a]
    } deriving (Show, Eq, Generic)

instance Validity a => Validity (TreeAbove a)

instance Functor TreeAbove where
    fmap f ta =
        TreeAbove
            { treeAboveLefts = map (fmap f) $ treeAboveLefts ta
            , treeAboveAbove = fmap (fmap f) $ treeAboveAbove ta
            , treeAboveNode = f $ treeAboveNode ta
            , treeAboveRights = map (fmap f) $ treeAboveRights ta
            }

makeTreeCursor :: Tree a -> TreeCursor a
makeTreeCursor (Node v fs) =
    TreeCursor {treeAbove = Nothing, treeCurrent = v, treeBelow = fs}

singletonTreeCursor :: a -> TreeCursor a
singletonTreeCursor v =
    TreeCursor {treeAbove = Nothing, treeCurrent = v, treeBelow = []}

rebuildTreeCursor :: TreeCursor a -> Tree a
rebuildTreeCursor TreeCursor {..} =
    (case treeAbove of
         Nothing -> id
         Just ta -> (\n -> go n ta))
        (Node treeCurrent treeBelow)
  where
    go :: Tree a -> TreeAbove a -> Tree a
    go t TreeAbove {..} =
        (case treeAboveAbove of
             Nothing -> id
             Just ta -> (\n -> go n ta))
            (Node treeAboveNode (treeAboveLefts ++ [t] ++ treeAboveRights))
