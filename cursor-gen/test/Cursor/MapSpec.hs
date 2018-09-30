{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Cursor.MapSpec
    ( spec
    ) where

import Test.Hspec
import Test.Validity

import Cursor.Map
import Cursor.Map.Gen ()

spec :: Spec
spec = do
    eqSpec @(MapCursor Word Int Bool Ordering)
    genValidSpec @(MapCursor Double Rational Int Bool)
    shrinkValidSpec @(MapCursor Double Rational Int Bool)
