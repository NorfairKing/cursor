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

instance GenUnchecked TreeCursorSelection
instance GenValid TreeCursorSelection

instance (GenUnchecked a, GenUnchecked b) => GenUnchecked (TreeCursor a b) where
    genUnchecked =
        sized $ \n -> do
            (a, b, c, d) <- genSplit4 n
            treeAbove <- resize (a + b) genUnchecked
            treeCurrent <- resize c genUnchecked
            treeBelow <- resize d genUnchecked
            pure TreeCursor {..}

instance (GenValid a, GenValid b) => GenValid (TreeCursor a b) where
    genValid =
        sized $ \n -> do
            (a, b, c, d) <- genSplit4 n
            treeAbove <- resize (a + b) genValid
            treeCurrent <- resize c genValid
            treeBelow <- resize d genValid
            pure TreeCursor {..}

instance GenUnchecked b => GenUnchecked (TreeAbove b) where
    genUnchecked =
        sized $ \n -> do
            (a, b, c, d) <- genSplit4 n
            treeAboveLefts <- resize a genUnchecked
            treeAboveAbove <- resize b genUnchecked
            treeAboveNode <- resize c genUnchecked
            treeAboveRights <- resize d genUnchecked
            pure TreeAbove {..}

instance GenValid b => GenValid (TreeAbove b) where
    genValid =
        sized $ \n -> do
            (a, b, c, d) <- genSplit4 n
            treeAboveLefts <- resize a genValid
            treeAboveAbove <- resize b genValid
            treeAboveNode <- resize c genValid
            treeAboveRights <- resize d genValid
            pure TreeAbove {..}
