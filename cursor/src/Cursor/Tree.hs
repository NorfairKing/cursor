{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Cursor.Tree where

import Data.Tree
import Data.Validity
import Data.Validity.Tree ()
import GHC.Generics (Generic)

data TreeCursor a = TreeCursor
    { treeAbove :: Maybe (TreeAbove a)
    , treeCurrent :: a
    , treeBelow :: Forest a
    } deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (TreeCursor a)

data TreeAbove a = TreeAbove
    { treeAboveLefts :: [Tree a]
    , treeAboveAbove :: Maybe (TreeAbove a)
    , treeAboveNode :: a
    , treeAboveRights :: [Tree a]
    } deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (TreeAbove a)

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
