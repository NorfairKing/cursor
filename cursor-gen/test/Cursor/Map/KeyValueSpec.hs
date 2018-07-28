{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Cursor.Map.KeyValueSpec
    ( spec
    ) where

import Test.Hspec

import Test.Validity
import Test.Validity.Optics

import Cursor.Map.KeyValue
import Cursor.Map.KeyValue.Gen ()

spec :: Spec
spec = do
    eqSpec @(KeyValueCursor Int Int)
    functorSpec @(KeyValueCursor Int)
    genValidSpec @(KeyValueCursor Double Double)
    eqSpec @KeyValueToggle
    genValidSpec @KeyValueToggle
    describe "makeKeyValueCursor" $
        it "produces valid cursors" $
        producesValidsOnValids2 (makeKeyValueCursor @Double @Double)
    describe "rebuildKeyValueCursor" $ do
        it "produces valid tuples" $
            producesValidsOnValids (rebuildKeyValueCursor @Double @Double)
        it "is the inverse of makeKeyValueCursor" $
            inverseFunctions
                (uncurry (makeKeyValueCursor @Int @Int))
                rebuildKeyValueCursor
    describe "keyValueCursorKeyL" $
        lensSpecOnValid (keyValueCursorKeyL @Double @Double)
    describe "keyValueCursorValueL" $
        lensSpecOnValid (keyValueCursorValueL @Double @Double)
    describe "keyValueCursorSelectKey" $
        it "produces valid cursors" $
        producesValidsOnValids (keyValueCursorSelectKey @Double @Double)
    describe "keyValueCursorSelectValue" $
        it "produces valid cursors" $
        producesValidsOnValids (keyValueCursorSelectValue @Double @Double)
    describe "keyValueCursorToggleSelected" $
        it "produces valid cursors" $
        producesValidsOnValids (keyValueCursorToggleSelected @Double @Double)
