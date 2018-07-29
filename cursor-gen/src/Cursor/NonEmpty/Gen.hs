{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.NonEmpty.Gen
    ( nonEmptyElemOf, nonEmptyWithIndex0
    ) where

import Data.GenValidity
import Test.QuickCheck

import qualified Data.List.NonEmpty as NE

import Cursor.NonEmpty

instance GenUnchecked a => GenUnchecked (NonEmptyCursor a)

instance GenValid a => GenValid (NonEmptyCursor a) where
    genValid = NonEmptyCursor <$> genValid <*> genValid <*> genValid

nonEmptyElemOf :: NonEmptyCursor a -> Gen a
nonEmptyElemOf = elements . NE.toList . rebuildNonEmptyCursor

nonEmptyWithIndex0 :: Gen a -> Gen (NonEmptyCursor a)
nonEmptyWithIndex0 g = NonEmptyCursor [] <$> g <*> genListOf g
