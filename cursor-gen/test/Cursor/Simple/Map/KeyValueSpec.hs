{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.Simple.Map.KeyValueSpec
  ( spec,
  )
where

import Cursor.Simple.Map.KeyValue
import Cursor.Simple.Map.KeyValue.Gen ()
import Test.Hspec
import Test.QuickCheck
import Test.Validity

spec :: Spec
spec = do
  describe "makeKeyValueCursorKey" $
    it "produces valid cursors" $
      producesValid2 (makeKeyValueCursorKey @Bool @Bool @Bool @Bool)
  describe "makeKeyValueCursorValue" $
    it "produces valid cursors" $
      producesValid2 (makeKeyValueCursorValue @Bool @Bool @Bool @Bool)
  describe "rebuildKeyValueCursor" $
    it "produces valid tuples" $
      producesValid (rebuildKeyValueCursor @Bool @Bool)
  describe "keyValueCursorSelection" $
    it "produces valid selections" $
      producesValid (keyValueCursorSelection @Bool @Bool @Bool @Bool)
  describe "keyValueCursorSelectKey" $ do
    it "produces valid cursors" $ producesValid (keyValueCursorSelectKey @Bool @Bool)
    it "is a movement" $ isMovement keyValueCursorSelectKey
  describe "keyValueCursorSelectValue" $ do
    it "produces valid cursors" $ producesValid (keyValueCursorSelectValue @Bool @Bool)
    it "is a movement" $ isMovement keyValueCursorSelectValue
  describe "keyValueCursorToggleSelected" $ do
    it "produces valid cursors" $ producesValid (keyValueCursorToggleSelected @Bool @Bool)
    it "is a movement" $ isMovement keyValueCursorToggleSelected

isMovement :: (forall k v. KeyValueCursor k v -> KeyValueCursor k v) -> Property
isMovement func =
  forAllValid $ \lec ->
    rebuildKeyValueCursor (lec :: KeyValueCursor Bool Bool)
      `shouldBe` rebuildKeyValueCursor (func lec)
