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

instance (GenUnchecked kc, GenUnchecked vc, GenUnchecked k, GenUnchecked v) =>
         GenUnchecked (MapCursor kc vc k v)

instance (GenValid kc, GenValid vc, GenValid k, GenValid v) =>
         GenValid (MapCursor kc vc k v) where
    genValid = genValidStructurally
    shrinkValid = shrinkValidStructurally
