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
    eqSpec @(KeyValueCursor Int Int Int Int)
    genValidSpec @(KeyValueCursor Double Double Double Double)
    eqSpec @KeyValueToggle
    genValidSpec @KeyValueToggle
