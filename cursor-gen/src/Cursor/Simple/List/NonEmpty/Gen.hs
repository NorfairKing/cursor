{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Simple.List.NonEmpty.Gen
    ( nonEmptyElemOf
    , nonEmptyWithIndex0
    ) where

import Test.QuickCheck

import qualified Cursor.List.NonEmpty.Gen as NEC
import Cursor.Simple.List.NonEmpty

nonEmptyElemOf :: NonEmptyCursor a -> Gen a
nonEmptyElemOf = NEC.nonEmptyElemOf

nonEmptyWithIndex0 :: Gen a -> Gen (NonEmptyCursor a)
nonEmptyWithIndex0 = NEC.nonEmptyWithIndex0
