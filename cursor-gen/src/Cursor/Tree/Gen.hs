{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Tree.Gen
    (
    ) where

import Data.GenValidity
import Data.GenValidity.Containers ()

import Cursor.Tree

instance GenUnchecked a => GenUnchecked (TreeCursor a)

instance GenValid a => GenValid (TreeCursor a) where
    genValid = TreeCursor <$> genValid <*> genValid <*> genValid

instance GenUnchecked a => GenUnchecked (TreeAbove a)

instance GenValid a => GenValid (TreeAbove a) where
    genValid = TreeAbove <$> genValid <*> genValid <*> genValid <*> genValid
