{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Map.Gen
    (
    ) where

import Test.QuickCheck

import Data.GenValidity
import Data.GenValidity.Containers ()

import Cursor.Map

import Cursor.NonEmpty.Gen ()

instance (GenUnchecked k, GenUnchecked v) => GenUnchecked (MapCursor k v)

instance (GenValid k, GenValid v) => GenValid (MapCursor k v) where
    genValid = (MapCursor <$> genValid) `suchThat` isValid

instance (GenUnchecked k, GenUnchecked v) =>
         GenUnchecked (KeyValueCursor k v)

instance (GenValid k, GenValid v) => GenValid (KeyValueCursor k v) where
    genValid = (KeyValueCursor <$> genValid <*> genValid) `suchThat` isValid
