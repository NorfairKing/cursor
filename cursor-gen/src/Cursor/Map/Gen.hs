{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Map.Gen
  ( genMapCursorBy,
    genMapCursorByDependent,
  )
where

import Cursor.List.NonEmpty.Gen
import Cursor.Map
import Cursor.Map.KeyValue.Gen
import Data.GenValidity
import Data.GenValidity.Containers ()
import Test.QuickCheck

instance (GenValid kc, GenValid vc, GenValid k, GenValid v) => GenValid (MapCursor kc vc k v) where
  genValid = genMapCursorBy genValid genValid genValid genValid
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

genMapCursorBy :: Gen kc -> Gen vc -> Gen k -> Gen v -> Gen (MapCursor kc vc k v)
genMapCursorBy genKC genVC genK genV =
  MapCursor
    <$> genNonEmptyCursorBy (genKeyValueCursorBy genKC genVC genK genV) ((,) <$> genK <*> genV)

genMapCursorByDependent :: Gen (kc, v) -> Gen (k, vc) -> Gen (k, v) -> Gen (MapCursor kc vc k v)
genMapCursorByDependent genKVCK genKVCV genKV =
  MapCursor <$> genNonEmptyCursorBy (genKeyValueCursorByDependent genKVCK genKVCV) genKV
