{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cursor.MapSpec
  ( spec,
  )
where

import Cursor.Map
import Cursor.Map.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  eqSpec @(MapCursor Bool Bool Bool Bool)
  genValidSpec @(MapCursor Bool Bool Bool Bool)
