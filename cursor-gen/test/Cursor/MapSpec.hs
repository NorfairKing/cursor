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
  eqSpec @(MapCursor Bool Bool Bool Bool)
  genValidSpec @(MapCursor Bool Bool Bool Bool)
