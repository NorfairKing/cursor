{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Map.KeyValue.Gen
    (
    ) where

import Test.QuickCheck

import Data.GenValidity

import Cursor.Map.KeyValue

instance (GenUnchecked k, GenUnchecked v) =>
         GenUnchecked (KeyValueCursor k v)

instance (GenValid k, GenValid v) => GenValid (KeyValueCursor k v) where
    genValid = (KeyValueCursor <$> genValid <*> genValid <*> genValid) `suchThat` isValid

instance GenUnchecked KeyValueToggle

instance GenValid KeyValueToggle
