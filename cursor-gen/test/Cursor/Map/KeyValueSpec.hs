{-# LANGUAGE TypeApplications #-}

module Cursor.Map.KeyValueSpec
  ( spec
  ) where

import Test.Hspec

import Test.Validity

import Cursor.Map.KeyValue
import Cursor.Map.KeyValue.Gen ()

spec :: Spec
spec = do
  eqSpec @(KeyValueCursor Bool Bool Bool Bool)
  genValidSpec @(KeyValueCursor Bool Bool Bool Bool)
  eqSpec @KeyValueToggle
  genValidSpec @KeyValueToggle
