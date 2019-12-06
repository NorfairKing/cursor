{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.Simple.Map.KeyValueSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import Cursor.Simple.Map.KeyValue
import Cursor.Simple.Map.KeyValue.Gen ()

spec :: Spec
spec = do
  describe "makeKeyValueCursorKey" $
    it "produces valid cursors" $
    producesValidsOnValids2 (makeKeyValueCursorKey @Bool @Bool @Bool @Bool)
  describe "makeKeyValueCursorValue" $
    it "produces valid cursors" $
    producesValidsOnValids2 (makeKeyValueCursorValue @Bool @Bool @Bool @Bool)
  describe "rebuildKeyValueCursor" $
    it "produces valid tuples" $ producesValidsOnValids (rebuildKeyValueCursor @Bool @Bool)
  describe "keyValueCursorSelection" $
    it "produces valid selections" $
    producesValidsOnValids (keyValueCursorSelection @Bool @Bool @Bool @Bool)
  describe "keyValueCursorSelectKey" $ do
    it "produces valid cursors" $ producesValidsOnValids (keyValueCursorSelectKey @Bool @Bool)
    it "is a movement" $ isMovement keyValueCursorSelectKey
  describe "keyValueCursorSelectValue" $ do
    it "produces valid cursors" $ producesValidsOnValids (keyValueCursorSelectValue @Bool @Bool)
    it "is a movement" $ isMovement keyValueCursorSelectValue
  describe "keyValueCursorToggleSelected" $ do
    it "produces valid cursors" $ producesValidsOnValids (keyValueCursorToggleSelected @Bool @Bool)
    it "is a movement" $ isMovement keyValueCursorToggleSelected

isMovement :: (forall k v. KeyValueCursor k v -> KeyValueCursor k v) -> Property
isMovement func =
  forAllValid $ \lec ->
    rebuildKeyValueCursor (lec :: KeyValueCursor Bool Bool) `shouldBe`
    rebuildKeyValueCursor (func lec)
