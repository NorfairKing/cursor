module Cursor.Simple.List.NonEmpty.Gen
  ( nonEmptyElemOf,
    nonEmptyWithIndex0,
    nonEmptyWith,
  )
where

import qualified Cursor.List.NonEmpty.Gen as NEC
import Cursor.Simple.List.NonEmpty
import Test.QuickCheck

nonEmptyElemOf :: NonEmptyCursor a -> Gen a
nonEmptyElemOf = NEC.nonEmptyElemOf

nonEmptyWithIndex0 :: Gen a -> Gen (NonEmptyCursor a)
nonEmptyWithIndex0 = NEC.nonEmptyWithIndex0

nonEmptyWith :: a -> Gen a -> Gen (NonEmptyCursor a)
nonEmptyWith = NEC.nonEmptyWith
