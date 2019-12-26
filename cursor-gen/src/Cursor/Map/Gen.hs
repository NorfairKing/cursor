{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Map.Gen
  ( genMapCursorBy
  ) where

import Data.GenValidity
import Data.GenValidity.Containers ()

import Test.QuickCheck

import Cursor.Map

import Cursor.List.NonEmpty.Gen
import Cursor.Map.KeyValue.Gen

instance (GenUnchecked kc, GenUnchecked vc, GenUnchecked k, GenUnchecked v) =>
         GenUnchecked (MapCursor kc vc k v) where
  genUnchecked = genMapCursorBy genUnchecked genUnchecked genUnchecked genUnchecked

instance (GenValid kc, GenValid vc, GenValid k, GenValid v) => GenValid (MapCursor kc vc k v) where
  genValid = genMapCursorBy genValid genValid genValid genValid
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

genMapCursorBy :: Gen kc -> Gen vc -> Gen k -> Gen v -> Gen (MapCursor kc vc k v)
genMapCursorBy genKC genVC genK genV =
  MapCursor <$>
  genNonEmptyCursorBy (genKeyValueCursorBy genKC genVC genK genV) ((,) <$> genK <*> genV)
