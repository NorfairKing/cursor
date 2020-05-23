{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Map.KeyValue.Gen
  ( genKeyValueCursorBy
  , genKeyValueCursorByDependent
  ) where

import Cursor.Map.KeyValue
import Data.GenValidity
import Test.QuickCheck

instance (GenUnchecked kc, GenUnchecked vc, GenUnchecked k, GenUnchecked v) =>
         GenUnchecked (KeyValueCursor kc vc k v) where
  genUnchecked = genKeyValueCursorBy genUnchecked genUnchecked genUnchecked genUnchecked

instance (GenValid kc, GenValid vc, GenValid k, GenValid v) =>
         GenValid (KeyValueCursor kc vc k v) where
  genValid = genKeyValueCursorBy genValid genValid genValid genValid
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked KeyValueToggle

instance GenValid KeyValueToggle

genKeyValueCursorBy :: Gen kc -> Gen vc -> Gen k -> Gen v -> Gen (KeyValueCursor kc vc k v)
genKeyValueCursorBy genKC genVC genK genV =
  oneof [KeyValueCursorKey <$> genKC <*> genV, KeyValueCursorValue <$> genK <*> genVC]

genKeyValueCursorByDependent :: Gen (kc, v) -> Gen (k, vc) -> Gen (KeyValueCursor kc vc k v)
genKeyValueCursorByDependent genKVCK genKVCV =
  oneof [uncurry KeyValueCursorKey <$> genKVCK, uncurry KeyValueCursorValue <$> genKVCV]
