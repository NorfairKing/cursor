{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Tree.Gen
    (
    ) where

import Test.QuickCheck

import Data.GenValidity
import Data.GenValidity.Containers ()

import Cursor.Tree

instance GenUnchecked a => GenUnchecked (TreeCursor a) where
    genUnchecked =
        sized $ \n -> do
            (a, b, c, d) <- genSplit4 n
            treeAbove <- resize (a + b) genUnchecked
            treeCurrent <- resize c genUnchecked
            treeBelow <- resize c genUnchecked
            pure TreeCursor {..}

instance GenValid a => GenValid (TreeCursor a) where
    genValid =
        sized $ \n -> do
            (a, b, c, d) <- genSplit4 n
            treeAbove <- resize (a + b) genValid
            treeCurrent <- resize c genValid
            treeBelow <- resize c genValid
            pure TreeCursor {..}

instance GenUnchecked a => GenUnchecked (TreeAbove a) where
    genUnchecked =
        sized $ \n -> do
            (a, b, c, d) <- genSplit4 n
            treeAboveLefts <- resize a genUnchecked
            treeAboveAbove <- resize b genUnchecked
            treeAboveNode <- resize c genUnchecked
            treeAboveRights <- resize d genUnchecked
            pure TreeAbove {..}

instance GenValid a => GenValid (TreeAbove a) where
    genValid =
        sized $ \n -> do
            (a, b, c, d) <- genSplit4 n
            treeAboveLefts <- resize a genValid
            treeAboveAbove <- resize b genValid
            treeAboveNode <- resize c genValid
            treeAboveRights <- resize d genValid
            pure TreeAbove {..}
