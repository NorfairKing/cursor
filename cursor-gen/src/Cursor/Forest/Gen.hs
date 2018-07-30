{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Forest.Gen
    (
    ) where

import Data.GenValidity
import Data.GenValidity.Containers ()

import Cursor.Forest

import Cursor.NonEmpty.Gen ()
import Cursor.Tree.Gen ()

instance GenUnchecked a => GenUnchecked (ForestCursor a)

instance GenValid a => GenValid (ForestCursor a) where
    genValid = ForestCursor <$> genValid
