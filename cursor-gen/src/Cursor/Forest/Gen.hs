{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Forest.Gen
  (
  )
where

import Cursor.Forest
import Cursor.List.NonEmpty.Gen ()
import Cursor.Tree.Gen ()
import Data.GenValidity
import Data.GenValidity.Containers ()

instance (GenValid a, GenValid b) => GenValid (ForestCursor a b) where
  genValid = ForestCursor <$> genValid
  shrinkValid (ForestCursor ne) = ForestCursor <$> shrinkValid ne
