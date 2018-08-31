{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Map.Gen
    (
    ) where

import Data.GenValidity
import Data.GenValidity.Containers ()

import Cursor.Map

import Cursor.List.NonEmpty.Gen ()
import Cursor.Map.KeyValue.Gen ()

instance (GenUnchecked k, GenUnchecked v) => GenUnchecked (MapCursor k v)

instance (GenValid k, GenValid v) => GenValid (MapCursor k v) where
    genValid = genValidStructurally
