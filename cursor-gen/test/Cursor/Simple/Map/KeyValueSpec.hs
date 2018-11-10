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
        producesValidsOnValids2
            (makeKeyValueCursorKey @Rational @Rational @Rational @Rational)
    describe "makeKeyValueCursorValue" $
        it "produces valid cursors" $
        producesValidsOnValids2
            (makeKeyValueCursorValue @Rational @Rational @Rational @Rational)
    describe "rebuildKeyValueCursor" $
        it "produces valid tuples" $
        producesValidsOnValids (rebuildKeyValueCursor @Rational @Rational)
    describe "keyValueCursorSelection" $
        it "produces valid selections" $
        producesValidsOnValids
            (keyValueCursorSelection @Rational @Rational @Rational @Rational)
    describe "keyValueCursorSelectKey" $ do
        it "produces valid cursors" $
            producesValidsOnValids (keyValueCursorSelectKey @Rational @Rational)
        it "is a movement" $ isMovement keyValueCursorSelectKey
    describe "keyValueCursorSelectValue" $ do
        it "produces valid cursors" $
            producesValidsOnValids (keyValueCursorSelectValue @Rational @Rational)
        it "is a movement" $ isMovement keyValueCursorSelectValue
    describe "keyValueCursorToggleSelected" $ do
        it "produces valid cursors" $
            producesValidsOnValids
                (keyValueCursorToggleSelected @Rational @Rational)
        it "is a movement" $ isMovement keyValueCursorToggleSelected

isMovement :: (forall k v. KeyValueCursor k v -> KeyValueCursor k v) -> Property
isMovement func =
    forAllValid $ \lec ->
        rebuildKeyValueCursor (lec :: KeyValueCursor Rational Rational) `shouldBe`
        rebuildKeyValueCursor (func lec)
