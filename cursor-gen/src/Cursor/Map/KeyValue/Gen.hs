{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Map.KeyValue.Gen
  (
  ) where

import Data.GenValidity

import Cursor.Map.KeyValue

instance (GenUnchecked kc, GenUnchecked vc, GenUnchecked k, GenUnchecked v) =>
         GenUnchecked (KeyValueCursor kc vc k v)

instance (GenValid kc, GenValid vc, GenValid k, GenValid v) =>
         GenValid (KeyValueCursor kc vc k v) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked KeyValueToggle

instance GenValid KeyValueToggle
