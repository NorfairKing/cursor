{-# LANGUAGE TypeApplications #-}

module Cursor.Map.KeyValueSpec
  ( spec,
  )
where

import Cursor.Map.KeyValue
import Cursor.Map.KeyValue.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  eqSpec @(KeyValueCursor Bool Bool Bool Bool)
  genValidSpec @(KeyValueCursor Bool Bool Bool Bool)
  eqSpec @KeyValueToggle
  genValidSpec @KeyValueToggle
