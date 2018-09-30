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
            (makeKeyValueCursorKey @Double @Rational @Double @Rational)
    describe "makeKeyValueCursorValue" $
        it "produces valid cursors" $
        producesValidsOnValids2
            (makeKeyValueCursorValue @Double @Rational @Double @Rational)
    describe "rebuildKeyValueCursor" $
        it "produces valid tuples" $
        producesValidsOnValids (rebuildKeyValueCursor @Double @Rational)
    describe "keyValueCursorSelection" $
        it "produces valid selections" $
        producesValidsOnValids
            (keyValueCursorSelection @Double @Rational @Double @Rational)
    describe "keyValueCursorSelectKey" $ do
        it "produces valid cursors" $
            producesValidsOnValids (keyValueCursorSelectKey @Double @Rational)
        it "is a movement" $ isMovement keyValueCursorSelectKey
    describe "keyValueCursorSelectValue" $ do
        it "produces valid cursors" $
            producesValidsOnValids (keyValueCursorSelectValue @Double @Rational)
        it "is a movement" $ isMovement keyValueCursorSelectValue
    describe "keyValueCursorToggleSelected" $ do
        it "produces valid cursors" $
            producesValidsOnValids
                (keyValueCursorToggleSelected @Double @Rational)
        it "is a movement" $ isMovement keyValueCursorToggleSelected

isMovement :: (forall k v. KeyValueCursor k v -> KeyValueCursor k v) -> Property
isMovement func =
    forAllValid $ \lec ->
        rebuildKeyValueCursor (lec :: KeyValueCursor Double Rational) `shouldBe`
        rebuildKeyValueCursor (func lec)
