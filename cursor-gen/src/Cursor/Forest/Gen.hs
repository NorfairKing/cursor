{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Forest.Gen
  (
  ) where

import Data.GenValidity
import Data.GenValidity.Containers ()

import Cursor.Forest

import Cursor.List.NonEmpty.Gen ()
import Cursor.Tree.Gen ()

instance (GenUnchecked a, GenUnchecked b) =>
         GenUnchecked (ForestCursor a b) where
  genUnchecked = ForestCursor <$> genUnchecked
  shrinkUnchecked (ForestCursor ne) = ForestCursor <$> shrinkUnchecked ne

instance (GenValid a, GenValid b) => GenValid (ForestCursor a b) where
  genValid = ForestCursor <$> genValid
  shrinkValid (ForestCursor ne) = ForestCursor <$> shrinkValid ne
