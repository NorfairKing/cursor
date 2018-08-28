{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Map.Gen
    (
    ) where

import Data.GenValidity
import Data.GenValidity.Containers ()

import Cursor.Map

import Cursor.Map.KeyValue.Gen ()
import Cursor.NonEmpty.Gen ()

instance (GenUnchecked k, GenUnchecked v) => GenUnchecked (MapCursor k v)

instance (GenValid k, GenValid v) => GenValid (MapCursor k v) where
    genValid = genValidStructurally
